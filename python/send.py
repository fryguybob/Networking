import socket
import os
import sys

server = sys.argv[1]
port = int(sys.argv[2])
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

m = input(socket.gethostname() + "> ")

print(m)

sock.sendto(bytes(m, 'utf-8'), (server, port))
