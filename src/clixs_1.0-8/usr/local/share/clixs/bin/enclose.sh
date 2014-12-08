#!/bin/bash
function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-H :: enable first line as header        (default=disabled)
		-u :: header unline character            (default=<top setting>)
		-w :: enclosure width <integer/percent>  (default=60%/120)
		-s :: side enclosure character           (default="##")
		-S :: side space gap <integer>           (default=1)
		-t :: top/bottom enclosure character     (default="#")
		-b :: bottom enclosure character <none>  (default=<top setting>)
		-B :: disable bottom enclosure line      (default=enabled)
	USAGE
}
function process_args(){
	COLUMNS=`tput cols`
	TOP="#"
	TOP_LINE='true'
	HEADER_LINE='false'
	BOTTOM_LINE='true'
	WIDTH_DEFAULT_PERCENT='60'
	WIDTH_DEFAULT='120'
	WRAP_INDENT='2'
		
	while getopts "h?Hu:w:s:S:t:b:B" OPT; do
		case "${OPT}" in
			h|\?)	usage
				exit;;
			H)	HEADER_LINE='true';;
			u)	HEADER_LINE='true'
				HEADER=${OPTARG};;
			w)	WIDTH=${OPTARG};;
			s)	SIDE=${OPTARG};;
			S)	SIDE_SPACE_CNT=${OPTARG};;
			t)	TOP=${OPTARG};;
			b)	BOTTOM=${OPTARG};;
			B)	BOTTOM_LINE='false';;
		esac
	done
	SIDE=${SIDE:-$(get_line 2 "${TOP}")}
	SIDE_SPACE_CNT=${SIDE_SPACE_CNT:-1}
	HEADER=${HEADER:-${TOP}}
	BOTTOM=${BOTTOM:-${TOP}}
	SIDE_SPACE=$(seq ${SIDE_SPACE_CNT} | sed "s/.*/ /" | tr -d \\\n)
	WIDTH=$(get_width)
	WRAP_WIDTH=$(( WIDTH - ( 2 * ( ${#SIDE} + SIDE_SPACE_CNT ) ) ))
}
function get_width(){
	if (( COLUMNS )) && ! (( ${#WIDTH} )); then
		echo $(( COLUMNS * WIDTH_DEFAULT_PERCENT / 100 ))
	elif (( COLUMNS )) && [ "${WIDTH: -1}" == "%" ]; then
		echo $(( COLUMNS * ${WIDTH:0:-1} / 100 ))
	elif ! (( ${#WIDTH} )) || [ "${WIDTH: -1}" == "%" ]; then
		echo ${WIDTH_DEFAULT}
	else
		echo ${WIDTH}
	fi
}
function wrap(){
	# arg1=WRAP_WIDTH, arg2=SPACE_INDENT_CNT; both have defaults
	local WRAP_WIDTH=${1:-$(( WIDTH - ( 2 * ( ${#SIDE} + SIDE_SPACE_CNT ) ) ))}
	local WRAP_INDENT_CNT=${2:-1}
	local WRAP_INDENT=$(seq ${WRAP_INDENT_CNT} | sed 's/.*/ /' | tr -d \\\n)
	# data must be piped
	sed 's/$/ /' |
	tr -d \\\n |
	sed "s/\(.\{1,${WRAP_WIDTH}\}\)[[:space:]]/\1\n/" |
	sed '1!'"{s/\(.\{1,$(( WRAP_WIDTH - WRAP_INDENT_CNT ))\}\)[[:space:]]/${WRAP_INDENT}\1\n/g}"
}
function get_line(){
	local LEN=$1
	shift
	local CHAR=$@
	local LINE=$(seq ${LEN} | sed "s/.*/${CHAR}/" | tr -d \\\n)
	local LINE=${LINE:0:${LEN}}
	echo "$LINE"
}
function main(){
	process_args "$@"

	# top line
	${TOP_LINE} && get_line ${WIDTH} "${TOP}"

	# first line
	while read line; do
		echo "${line}"				|	
		wrap ${WRAP_WIDTH} ${WRAP_INDENT}	|
		awk -vW=$(( WIDTH - ${#SIDE} ))		\
		    -vSS="${SIDE_SPACE}"		\
		    -vS="${SIDE}"			\
		    '{printf "%-" W "s%s\n", S SS $0, S}'
		break
	done

	# underline
	${HEADER_LINE} && get_line ${WIDTH} "${HEADER}"

	# body
	while read line; do
		echo "${line}"				|	
		wrap ${WRAP_WIDTH} ${WRAP_INDENT}	|
		awk -vW=$(( WIDTH - ${#SIDE} ))		\
		    -vSS="${SIDE_SPACE}"		\
		    -vS="${SIDE}"			\
		    '{printf "%-" W "s%s\n", S SS $0, S}'
	done

	# bottom line
	${BOTTOM_LINE} && get_line ${WIDTH} "${BOTTOM}"
}
main "$@"
