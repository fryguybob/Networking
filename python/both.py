import socket
import sys
import select

listen = "0.0.0.0" # socket.gethostname()
port = int(sys.argv[2])
sockL = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sockL.bind((listen, port))
sockL.setblocking(0)

server = sys.argv[1]
sockS = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sockS.setblocking(0)

inputs = [ sys.stdin, sockL ]
outputs = [ ]

# m = input(socket.gethostname() + "> ")

while inputs:
    print("> ")
    
    readable, writeable, exceptional = select.select(inputs, outputs, inputs)
    
    for s in readable:
      if s == sys.stdin:
        m = sys.stdin.readline()
        sockS.sendto(bytes(m, 'utf-8'), (server, port))
      else:
        data, addr = s.recvfrom(256)
        print(str(data, 'utf-8'))

