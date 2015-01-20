#!/usr/bin/python
import socket
s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
# iMac 10.173.119.90 00:1e:c2:03:07:d4 
mac='\x00\x1e\xc2\x03\x07\xd4'
ip='10.173.119.90'
bcast='10.173.119.127'

try:
	#s.sendto('\xff' * 6 + mac * 16, (ip, 80))
	s.sendto('\xff' * 6 + mac * 16, (bcast, 9))
	#s.sendto('\xff' * 6 + '\x00\x1e\xc2\x03\x07\xd4' * 16, ('10.173.119.90', 80))
	#s.sendto('\xff' * 6 + '\x00\x1e\xc2\x03\x07\xd4' * 16, ('10.173.119.127', 9))
except IOError as e:
	print "Majic Packet Error :: " + e.strerror
else:
	print "Majic Packet Sent Successfully"
