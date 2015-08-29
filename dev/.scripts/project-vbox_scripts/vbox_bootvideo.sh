#!/bin/bash

function main(){
	local NAME='Ubuntu-1404-ZFS'
	local STOP='120'

	#local NAME='Ubuntu-1404-apt-cache'

	while true; do
		case $(vbox_get_vm_state "${NAME}") in
			poweroff)	echo PROCEEDING: Virtual Machine \"${NAME}\" is powered-off and ready.
					break;;
			aborted)	echo STAND-BY: Virtual Machine \"${NAME}\" is state: aborted\; power cycling.
					nohup vboxheadless -v off -s "${NAME}" &
					sleep 1
					vboxmanage controlvm "${NAME}" poweroff
					sleep 1;;
			*)		echo -n ERROR: Virtual Machine \"${NAME}\" is state:\ 
					vbox_get_vm_state "${NAME}"
					exit 1;;
		esac
	done

	
	echo STARTING: Virtual Machine \"${NAME}\" in order to enable video capture.
	nohup vboxheadless -v off -s "${NAME}" &
	sleep .5

	
	echo PAUSING: Virtual Machine \"${NAME}\".
	while ! vbox_is_machine_paused "${NAME}"; do
		vboxmanage controlvm "${NAME}" pause
		sleep .5
		vbox_is_machine_poweroff "${NAME}" && exit 2
	done
	

	echo ENABLING: Virtual Machine \"${NAME}\" video capture to webm format.
	vboxmanage controlvm "${NAME}" vcpenabled on
	sleep .5

	vboxmanage showvminfo "${NAME}" -machinereadable | grep vcp
	vboxmanage showvminfo "${NAME}" -machinereadable | grep -i state
	sleep .5

	echo CYCLING: Virtual Machine \"${NAME}\" state off/on.
	vboxmanage controlvm "${NAME}" resume
	vboxmanage controlvm "${NAME}" reset
	local TIMER=$(now 0)
	sleep .5

	echo VERIFYING: Virtual Machine \"${NAME}\" is running.
	while ! vbox_is_machine_running "${NAME}"; do
		sleep .5
		vbox_is_machine_poweroff "${NAME}" && exit 3
	done
	sleep .5

	echo ENABLING: Virtual Machine \"${NAME}\" video read flag.
	vbox_chmod_vm_vcp_files "${NAME}"

	echo CAPTURING: Virtual Machine \"${NAME}\" for the next ${STOP} seconds.
	while (( TIMER + STOP > $(now 0) )); do
		sleep 1
		echo -n $(( TIMER + STOP - $(now 0) ))'>>'
	done
	echo

	echo DISABLING: Virtual Machine \"${NAME}\" video capture.
	vboxmanage controlvm "${NAME}" vcpenabled off

}
. vbox_functions.sh
main "$@"

