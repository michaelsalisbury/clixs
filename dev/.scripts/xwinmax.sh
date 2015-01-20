#!/bin/bash

function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-w :: pad width in pixels
		-h :: pad height in pixels
		-p :: print as WIDTH= HEIGHT= variables
		-x :: print as WIDTHxHEIGHT (default)
	USAGE
}
function process_args(){
	PAD_WIDTH='0'
	PAD_HEIGHT='0'
	PRINT_FORMAT='true'
	set -- "${@/--help/-u}"
	while getopts "ph:u?w:x" OPT; do
		case "${OPT}" in
			u|\?)	usage
				exit;;
			p)	PRINT_FORMAT='false';;
			w)	PAD_WIDTH="${OPTARG}";;
			h)	PAD_HEIGHT="${OPTARG}";;
		esac
	done
}


function get_max_window_geometry(){
	TITLE="$$_${RANDOM}_GET_MAX_WINDOW_GEOMETRY"
	gnome-terminal --maximize -x bash -c "wmctrl -r :ACTIVE: -T ${TITLE}; xwininfo -name ${TITLE} | tee /dev/shm/${TITLE} &>/dev/null" &>/dev/null
	WIDTH=$(awk '/Width:/{print $2}' "/dev/shm/${TITLE}")
	HEIGHT=$(awk '/Height:/{print $2}' "/dev/shm/${TITLE}")
	rm  "/dev/shm/${TITLE}"
}
function main(){
	process_args "$@"
	get_max_window_geometry
	
	WIDTH=$((  WIDTH  + PAD_WIDTH ))
	HEIGHT=$(( HEIGHT + PAD_HEIGHT ))
	if $PRINT_FORMAT; then
		echo $WIDTH"x"$HEIGHT
	else
		echo WIDTH=${WIDTH}
		echo HEIGHT=${HEIGHT}
	fi
}
main "$@"
