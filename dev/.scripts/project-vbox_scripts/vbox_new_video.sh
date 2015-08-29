#!/bin/bash

function main(){
	local NAME='Ubuntu-1404-ZFS'
	local STOP='120'

	#local NAME='Ubuntu-1404-apt-cache'

	while true; do
		case $(vbox_get_vm_state "${NAME}") in
			running)	echo PROCEEDING: Virtual Machine \"${NAME}\" is running.
					case "$(vbox_get_vminfo "${NAME}" vcpenabled)" in
						on)	echo vcpenabled: on, disabling.
							vboxmanage controlvm "${NAME}" vcpenabled off;;
						off)	echo vcpenabled: off, enabling.
							vboxmanage controlvm "${NAME}" vcpenabled on
							echo ENABLING: Virtual Machine \"${NAME}\" video read flag.
							vbox_chmod_vm_vcp_files "${NAME}"
							exit 1;;
					esac
					echo STATUS: Video capture is ... $(vbox_get_vminfo "${NAME}" vcpenabled). Done.;;
			*)		echo STAND-BY: Virtual Machine \"${NAME}\" is state: $(vbox_get_vm_state "${NAME}").
					exit 1;;
		esac
	done

}
. vbox_functions.sh
main "$@"

