#!/bin/bash

function main(){
	#process_args "$@"

	local VM_NAME="Ubuntu-1404-ZFS"
	local VM_LOG=$(vbox_get_vm_log_file "${VM_NAME}")

	{
		local TIME=$(date "+%T.%N")
		tail -n 0 -f "${VM_LOG}" &
		echo ${TIME:0: -3} tail_pid $!
	} |
	while read LINE; do
		#echo "${LINE}"
		# capture the tail PID in order to exit loop when triggered
		if [[ "${LINE}" =~ " tail_pid "  ]]; then
			tail_pid=${LINE##* }
			echo -------------------------------------------------${tail_pid}
		fi
		# run commands when state changes
		if [[ "${LINE}" =~ "Changing the VM state from" ]]; then
			echo -------------------------------------------------filter
			while read TIME OLD_STATE NEW_STATE; do
				case "${NEW_STATE//[^[:alpha:]_]/}" in
					RUNNING)	echo STARTING
							./vbox_screenshots.sh -l 1 -t 130 -s 20 -i 750 "${VM_NAME}" &
							;;
					TERMINATED)	kill ${tail_pid};;
				esac
			done < <(awk '{print $1,$7,$NF}' <<< "${LINE}")
		fi
	done
}



function process_args(){
	if [ -z "$*" ]; then
		return
	fi

	# set switch/arg defaults
	`switch_set_default a ${GV_SLEEP_ACCURACY}`
	`switch_set_default i ${GV_SCREENSHOT_INT}`
	`switch_set_default l ${GV_PROGRAM_TTL}`
	`switch_set_default s ${GV_SCRUB_INT}`
	`switch_set_default t ${GV_SCREENSHOT_TTL}`

	#switches_verifier "$@"
	local OPTIND=
	local OPTARG=
	local OPTION=
	local OPTERR=1
	while getopts "a:i:l:ns:t:" OPTION; do
		local switches_last_option=$OPTION
		local switches_last_optarg=$OPTARG

		# soucre dependancies if nessisary
		case $OPTION in
			a)	GV_SLEEP_ACCURACY=${OPTARG};;
			i)	GV_SCREENSHOT_INT=${OPTARG};;
			l)	GV_PROGRAM_TTL=${OPTARG};;
			n)	GV_SCREENSHOT_NEW='false';;
			s)	GV_SCRUB_INT=${OPTARG};;
			t)	GV_SCREENSHOT_TTL=${OPTARG};;
			?)	;;
		esac
	done
	# Shift to non parced command line arguments
	shift $(( OPTIND - 1 ))

	local VM_NAME
	for VM_NAME in "$@"; do
		if vbox_is_machine_running "${VM_NAME}"; then
			GV_SCREENSHOT_ALL='false'
			GV_SCREENSHOT_NEW='false'
			GV_VM_SNAP_LIST+=${GV_VM_SNAP_LIST:+$'\n'}"${VM_NAME}"
		fi
	done
}
source vbox_functions.sh
main "$@"
