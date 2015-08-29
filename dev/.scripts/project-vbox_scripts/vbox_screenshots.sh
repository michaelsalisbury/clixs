#!/bin/bash

# GLOBAL DEFAULTS
	GV_PROGRAM_TTL='3'		# arg -l in minutes 
	GV_SCREENSHOT_TTL='300'		# arg -t in seconds
	GV_SCRUB_INT='30'		# arg -s in seconds
	GV_SCREENSHOT_INT='750'		# arg -i in miliseconds
	GV_SCREENSHOT_DIR="VirtualBox VM Screenshots"
	GV_SCREENSHOT_ALL='true'	# sets false if valid running vbox machine names supplied
	GV_SCREENSHOT_NEW='true'	# arg -n sets false and no newly started VMs will be included
	GV_NEW_VM_CHECK_INT='7'		# in seconds
	GV_SLEEP_ACCURACY='4'		# arg -a integer 1 to 50, higher is more costly

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
		if vbox_does_machine_exist "${VM_NAME}"; then
			GV_SCREENSHOT_ALL='false'
			GV_SCREENSHOT_NEW='false'
			GV_VM_SNAP_LIST+=${GV_VM_SNAP_LIST:+$'\n'}"${VM_NAME}"
		fi
	done
}
function main(){
	process_args "$@"
	local SLEEP_INT=$(echo scale=3\; ${GV_SCREENSHOT_INT:-500} \/ ${GV_SLEEP_ACCURACY:-3} \/ 1000 | bc)
	local EXIT_TIME=$(( GV_PROGRAM_TTL * 60 + $(now 0) ))
	local NEXT_SCRUB=$(( GV_SCRUB_INT + $(now 0) ))
	local NEXT_JPG=$(( GV_SCREENSHOT_INT + $(now 3) ))
	local NEXT_NEW=$(( GV_NEW_VM_CHECK_INT + $(now 0) ))
	local GV_SCREENSHOT_TTL=$(echo scale=2\; $GV_SCREENSHOT_TTL \/ 60 | bc)
	if $GV_SCREENSHOT_ALL; then
		load GV_VM_SNAP_LIST vbox_list_running_names
	fi
	vbox_prep_screenshot_destination_root
	vbox_prep_screenshot_destinations
	
	while true; do
		local NOW=$(now 3)
		local NOW0=${NOW:0: -3}
		# test for exit
		if ((  NOW0 > EXIT_TIME )); then
			echo; echo break
			break
		# test for snapshot
		elif (( NOW > NEXT_JPG )); then
			vbox_take_screenshots
			let NEXT_JPG+=$GV_SCREENSHOT_INT
		# check for new VM started since snapshot launch
		elif $GV_SCREENSHOT_NEW && (( NOW0 > NEXT_NEW )); then
			load GV_VM_SNAP_LIST vbox_list_running_names
			vbox_prep_screenshot_destinations
			echo; echo reset GV_VM_SNAP_LIST
			let NEXT_NEW+=$GV_NEW_VM_CHECK_INT
		# test for scrub
		elif ((  NOW0 > NEXT_SCRUB )); then
			echo
			vbox_scrub_screenshots
			let NEXT_SCRUB+=$GV_SCRUB_INT
		# sleep as needed
		else
			echo -n z
			sleep ${SLEEP_INT}
		fi
	done
}
function vbox_take_screenshots(){
	while read VM_NAME; do
		vboxmanage controlvm "${VM_NAME}" screenshotpng "${GV_SCREENSHOT_DIR}/${VM_NAME}/${VM_NAME}.${NOW}.png"
		ln -f "${GV_SCREENSHOT_DIR}/${VM_NAME}/${VM_NAME}.${NOW}.png" "${GV_SCREENSHOT_DIR}/${VM_NAME}/${VM_NAME}.png"
		#echo ${VM_NAME}
		echo -n @
	done <<< "${GV_VM_SNAP_LIST}"
}
function vbox_scrub_screenshots(){
	while read VM_NAME; do
		#find "${GV_SCREENSHOT_DIR}/${VM_NAME}" -mmin +${GV_SCREENSHOT_TTL} -exec ls -1 '{}' ';'
		find "${GV_SCREENSHOT_DIR}/${VM_NAME}" -mmin +${GV_SCREENSHOT_TTL} -exec rm -vf '{}' ';'
	done <<< "${GV_VM_SNAP_LIST}"
}
function vbox_prep_screenshot_destination_root(){
	local VB_HOME=$(find ${HOME} -name VirtualBox.xml | xargs dirname)
	if [[ "${GV_SCREENSHOT_DIR}" =~ \/ ]] && [ -d "${GV_SCREENSHOT_DIR%/*}" ]; then
		mkdir -p "${GV_SCREENSHOT_DIR}"
	elif ! [[ "${GV_SCREENSHOT_DIR}" =~ \/ ]]; then
		GV_SCREENSHOT_DIR="${VB_HOME}/${GV_SCREENSHOT_DIR}"
		mkdir -p "${GV_SCREENSHOT_DIR}"
	else
		exit 1
	fi
}
function vbox_prep_screenshot_destinations(){
	if [ -d "${GV_SCREENSHOT_DIR}" ]; then
		while read VM_NAME; do
			if [ ! -d "${GV_SCREENSHOT_DIR}/${VM_NAME}" ]; then
				mkdir -p "${GV_SCREENSHOT_DIR}/${VM_NAME}"
			fi
		done <<< "${GV_VM_SNAP_LIST}"
	fi
}
. vbox_functions.sh
main "$@"
