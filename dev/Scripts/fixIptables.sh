#!/bin/bash

# clean iptables
iptables -F
iptables -X
iptables -t nat -F
iptables -t nat -X
iptables -t mangle -F
iptables -t mangle -X
iptables -P INPUT ACCEPT
iptables -P FORWARD ACCEPT
iptables -P OUTPUT ACCEPT

sleep 3

# -A PREROUTING -d 10.173.119.78 -i eth0 -p tcp -m tcp --dport 53022 -j DNAT --to-destination 192.168.0.3:22
# -A PREROUTING -d 10.173.119.78 -i eth1 -p tcp -m tcp --dport 53022 -j DNAT --to-destination 192.168.0.112:22

# Setup NAT from eth1 (private) to eth0 (public)
iptables --table nat --append POSTROUTING --out-interface eth0 -j MASQUERADE
iptables --append FORWARD --in-interface eth1 -j ACCEPT
iptables --append FORWARD --in-interface eth1:0 -j ACCEPT
iptables --append FORWARD --in-interface eth1:1 -j ACCEPT

iptables --table nat --append PREROUTING -d 10.173.119.78 -i eth0 -p tcp -m tcp --dport 53022 -j DNAT --to-destination 192.168.0.112:22
#iptables --append PREROUTING -d 10.173.119.78 -i eth1 -p tcp -m tcp --dport 53022 -j DNAT --to-destination 192.168.0.112:22
#iptables --append FORWARD -s 192.168.0.0/24 -d 0.0.0.0/0 -j ACCEPT
#iptables --append FORWARD -s 0.0.0.0/0 -d 192.168.0.0/24 -j ACCEPT
iptables --append INPUT -i eth0 -j ACCEPT


echo 1 > /proc/sys/net/ipv4/ip_forward
iptables -L
iptables --list-rules
iptables -L -t nat
iptables --list-rules -t nat
iptables -L -t mangle
iptables --list-rules -t mangle
