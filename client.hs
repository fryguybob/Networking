{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Zipper   as Z
import qualified Data.Text.Encoding as E
import qualified Data.Vector        as V
import qualified Data.Map           as M
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Data.List (isPrefixOf)

import Brick
import Brick.BChan
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.List

import Control.Concurrent
import Control.Lens
import Control.Monad (forM_, void)
import Control.Monad.IO.Class

import Graphics.Vty

import Network.HostName
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB

import System.Exit
import System.Environment

data Name = Send
          | Messages
    deriving (Ord, Eq, Enum, Bounded, Show)

newtype NoPort = NoPort N.SockAddr

instance Ord NoPort where
  NoPort (N.SockAddrInet _ a)  `compare` NoPort (N.SockAddrInet _ b)  = a `compare` b     
  NoPort (N.SockAddrInet6 _ _ a _) `compare`
    NoPort (N.SockAddrInet6 _ _ b _) = a `compare` b   
  NoPort a `compare` NoPort b = a `compare` b

instance Eq NoPort where
  a == b = a `compare` b == EQ

data NetworkSettings
    = NetworkSettings
      { _clients     :: [(N.Socket, N.SockAddr)]
      , _server      :: Maybe (N.Socket, N.SockAddr)
      , _description :: T.Text
      , _names       :: M.Map NoPort T.Text
      }

data State = State
    { _hostName :: !T.Text
    , _send     :: !(Editor T.Text Name)
    , _messages :: !(List Name T.Text)
    , _focus    :: !(FocusRing Name)
    , _network  :: !NetworkSettings
    }

data MessageEvent = MessageEvent (Maybe N.SockAddr) T.Text

makeLenses ''NetworkSettings
makeLenses ''State

-- ----------------------------- UI ---------------------------------
initialState h n = State
    h
    (editorText Send (Just 1) "")
    (list Messages V.empty 1)
    (focusRing [(minBound :: Name) ..])
    n

handleW :: State -> BrickEvent n e -> EventM Name (Next State)
handleW s e =
    case focusGetCurrent (s^.focus) of
      Just Send -> handleSend s e
      Just Messages -> handleMessages s e
      _ -> continue s

handleMessages :: State -> BrickEvent n e -> EventM Name (Next State)
handleMessages s (VtyEvent e)
    = continue =<< handleEventLensed s messages handleListEvent e

handleSend :: State -> BrickEvent n e -> EventM Name (Next State)
handleSend s (VtyEvent e)
    = continue =<< handleEventLensed s send handleEditorEvent e

handleSendMessage :: State -> EventM Name (Next State)
handleSendMessage s
    | T.isPrefixOf ":" m = runCommand m s'
    | otherwise          = do
        liftIO $ case s^.network.server of
            Just (sock, addr) -> void $ NB.sendTo sock (E.encodeUtf8 m) addr
            Nothing           -> forM_ (s^.network.clients) $ \(c, addr) -> do
                                     NB.sendTo c t' addr
        continue $ s' & echo
  where s' = s & send %~ applyEdit Z.clearZipper
        m = mconcat $ getEditContents (s^.send)
        t = mconcat $ s^.hostName : ": " : [m]
        t'= E.encodeUtf8 t
        l = s^.messages.listElementsL.to length
        add = listInsert l t
        remove = if l >= maxMessages
                   then listRemove 0
                   else id
        scroll = if focusGetCurrent (s^.focus) == Just Messages
                   then id
                   else listMoveTo (min (l+1) maxMessages)
        echo = if s^.network.server.to (not . isJust)
                 then messages %~ scroll . remove . add
                 else id


maxMessages = 1000

runCommand :: T.Text -> State -> EventM Name (Next State)
runCommand cmd s
    | cmd == ":q"     = halt s
    | cmd == ":clear" = continue $ s & messages %~ listClear
    | otherwise       = continue s

handleEvent :: State -> BrickEvent n MessageEvent -> EventM Name (Next State)
handleEvent s e@(VtyEvent (EvKey key [])) =
    case key of
      KEsc       -> halt s
      KChar '\t' -> continue (s & focus %~ focusNext)
      KEnter     -> handleSendMessage s
      _          -> handleW s e
handleEvent s e@(VtyEvent (EvKey key [MShift])) =
    case key of
      KChar '\t' -> continue (s & focus %~ focusPrev)
      _          -> handleW s e
handleEvent s e@(AppEvent (MessageEvent src m)) = do
    let cs = s^.network.clients
    liftIO $ forM_ cs $ \(c, addr) -> do
        NB.sendTo c t' addr
    continue $ s & messages %~ scroll . remove . add
  where
    t = case src of
          Just a  -> case M.lookup (NoPort a) (s^.network.names) of
                        Just n  -> n <> ": " <> m
                        Nothing -> T.pack (show a) <> ": " <> m 
          Nothing -> m
    t'= E.encodeUtf8 t
    l = s^.messages.listElementsL.to length
    add = listInsert l t
    remove = if l >= maxMessages
               then listRemove 0
               else id
    scroll = if focusGetCurrent (s^.focus) == Just Messages
               then id
               else listMoveTo (min (l+1) maxMessages)

handleEvent s e = handleW s e
    
draw :: State -> [Widget Name]
draw st = pure $ borderWithLabel (txt "Messages") m <=>
                 borderWithLabel (txt ("Send (" <> st^.hostName <> " -- "
                                                <> st^.network.description <> ")")) s
  where
    m = withFocusRing (st^.focus) (renderList (\_ e -> txt e)) (st^.messages)
    s = vLimit 1 $ withFocusRing (st^.focus) (renderEditor (txt . T.unlines)) (st^.send)

attrs :: AttrMap
attrs = attrMap defAttr [ (listSelectedFocusedAttr, green `on` black)
                        ]

app = App
    { appDraw = draw
    , appStartEvent = return
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrs
    , appChooseCursor = focusRingCursor (^.focus)
    }

-- -------------------------- Net code -----------------------

listenThread :: Bool -> BChan MessageEvent -> N.Socket -> IO ()
listenThread tag chan sock = go 
  where
    go = do
      (m, addr) <- NB.recvFrom sock 140
      writeBChan chan $ MessageEvent (if tag then Just addr else Nothing)
                                     (E.decodeUtf8 m)
      go

startListen :: Bool -> BChan MessageEvent -> String -> IO T.Text
startListen server chan port = do
    let hints = N.defaultHints { N.addrFlags = [N.AI_PASSIVE]
                               , N.addrSocketType = N.Datagram
                               }
    addr:_ <- N.getAddrInfo (Just hints) Nothing (Just port)
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.bind sock (N.addrAddress addr)
    desc <- T.pack . show <$> N.getSocketName sock

    forkIO $ listenThread server chan sock
    return desc

getSendSock :: String -> String -> IO (N.Socket, N.SockAddr)
getSendSock host port = do
    let hints = N.defaultHints { N.addrFlags = [N.AI_ALL]
                               , N.addrSocketType = N.Datagram
                               }
    addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    return (sock, N.addrAddress addr)

startServer :: BChan MessageEvent -> String -> FilePath -> IO NetworkSettings
startServer chan port clients = do
    cs <- lines <$> readFile clients
    cs' <- mapM (flip getSendSock port) cs
    let m = M.fromList $ zip (map (NoPort . snd) cs') (map T.pack cs)

    desc <- startListen True chan port

    return $ NetworkSettings cs' Nothing ("Hosting on " <> desc) m

startClient :: BChan MessageEvent -> String -> String -> IO NetworkSettings
startClient chan server port = do
    c@(sock, addr) <- getSendSock server port
    let desc = T.pack . show $ addr

    _ <- startListen False chan port

    return $ NetworkSettings [] (Just c) ("Sending to " <> desc) M.empty

main = do
    as <- getArgs
    chan <- newBChan 10
    net <- case as of
        ["--server", n, clients] -> startServer chan n clients
        [server, n]
          | not (isPrefixOf "-" server) -> startClient chan server n
        _ -> do
            p <- getProgName
            die $ "Usage: " <> p <> " --server [port] [client-file]\n"
               <> "       " <> p <> " [server-name] [port]"
    host <- T.pack <$> getHostName
    _ <- customMain
            (mkVty defaultConfig)
            (Just chan) app (initialState host net)
    return ()
