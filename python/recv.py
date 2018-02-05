import socket
import sys

server = "0.0.0.0" # socket.gethostname()
port = int(sys.argv[1])
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((server, port))

while True:
  data, addr = sock.recvfrom(256)
  print(str(data, 'utf-8'))
