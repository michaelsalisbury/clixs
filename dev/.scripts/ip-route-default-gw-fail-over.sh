#!/bin/bash

# in order for this script to work one or more default gateways with a metric of 1 or higher must be set
# add multiple default gateways in /etc/network/interfaces
# assign gateways a metric even if they were configured via dhcp; as follows:
#   remove gateway :: up route del default gw 192.168.248.248 || true
#   re-add gateway :: post-up route add default gw 192.168.248.248 metric 1 || true
#   i down rem gw  :: pre-down route del default gw 192.168.248.248 || true

ip route list			|
grep ^default.*metric	|
sort -k7				|
while read x x ip x dev x metric; do

  echo

  echo pinging default GW :: default via $ip dev $dev metric $metric

  echo && ping -c 1 $ip || continue

  (
    ip route list | grep default | grep -v metric &&
    ip route list | grep default | grep -v metric | xargs ip route del
  ) | xargs echo removing default GW ::

  echo setting default GW :: ip route add default via $ip dev $dev  metric $metric

  echo && ip route add default via $ip dev $dev
  
  echo testing default GW :: $(ip route list | grep ^default | sort -k7 | head -1 | cut -d\  -f3)
  
  echo && host google.com && echo && ping -c 1 google.com && break

done


