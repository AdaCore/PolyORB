#!/usr/local/bin/python
#

from socket import *
from select import select

s = socket(AF_INET, SOCK_STREAM)
s.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
s.bind(('', 8000))
s.listen(5)

s1, fr = s.accept()
print "Accepted one socket from %s"%`fr`

s2, fr = s.accept()
print "Accepted another socket from %s"%`fr`

while 1:
    r, w, e = select([s1, s2], [], [])
    if s1 in r:
	b = s1.recv(1500)
	if not b:
	    print "No more data on first socket"
	    break
	s2.send(b)
	print "First -> Second (%d bytes)"%len(b)
    if s2 in r:
	b = s2.recv(1500)
	if not b:
	    print "No more data on second socket"
	    break
	s1.send(b)
	print "Second -> First (%d bytes)"%len(b)
