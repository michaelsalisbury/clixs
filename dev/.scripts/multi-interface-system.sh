#!/bin/sh
#
# This script will be executed *after* all the other init scripts.
# You can put your own initialization stuff in here if you don't
# want to do the full Sys V style init stuff.

touch /var/lock/subsys/local

#mbm - 04/16/2012 - add additional routes
for interface_file in $(ls /etc/sysconfig/network-scripts/ifcfg-eth* | grep -v ifcfg-eth0) ;do
	. ${interface_file}
	prefix=$(ipcalc -p ${IPADDR} ${NETMASK} | awk -F= '{print $2}')
	tablenum=$(echo ${DEVICE} | sed 's/eth//g')
	if [ ${ONBOOT} != 'yes' ] ;then
		continue
	fi
	if ! grep "^${tablenum} ${DEVICE}$" /etc/iproute2/rt_tables >/dev/null ;then
		echo "${tablenum} ${DEVICE}" >>/etc/iproute2/rt_tables
	fi
	ip route add ${NETWORK}/${prefix} dev ${DEVICE} src ${IPADDR} table ${DEVICE}
	ip route add default via ${ROUTER} dev ${DEVICE} table ${DEVICE}
	ip rule add from ${IPADDR}/32 table ${DEVICE}
	ip rule add to ${IPADDR}/32 table ${DEVICE}
done
