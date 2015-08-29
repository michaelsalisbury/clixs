#!/bin/bash

function main(){
	# VARS
	local NAME NIC INT INTS MAC NET
	# Bounce networks
	vbox_list_running_names |
	while read NAME; do
		vbox_get_vm_adapters_bridged "${NAME}" |
		while read NIC; do
			vbox_get_vm_adapter_bridged_int "${NAME}" ${NIC}
		done
	done |
	sort -u |
	while read INT; do
		NET=$(ip -o -4 addr show ${INT} | awk '{print $4}')
		if nmap_bounce_net ${NET} 22,3389; then
			echo nmap bounce of ${INT}:${NET} successfull.
		else
			echo nmap bounce of ${INT}:${NET} failed.
		fi
	done
	
	# Get VM IP addresses for bridged interfaces
	vbox_list_running_names |
	while read NAME; do
		vbox_get_vm_adapters_bridged "${NAME}" |
		while read NIC; do
			INT=$(vbox_get_vm_adapter_bridged_int "${NAME}" ${NIC})
			MAC=$(vbox_get_vm_adapter_mac         "${NAME}" ${NIC} :)
			NET=$(ip -o -4 addr show ${INT} | awk '{print $4}')
			IP=$(arp_mac_to_ip ${INT} ${MAC})
			if ! nmap_is_host_up ${IP} 22,3389; then
				unset IP	
			fi
			echo ${NAME} = ${NIC} ${INT} ${MAC} ${IP}
		done
	done
}
. vbox_functions.sh
main "$@"

