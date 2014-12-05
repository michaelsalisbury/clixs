#!/bin/bash
function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-H :: enable first line as header        (default=disabled)
		-u :: header unline character            (default=<top setting>)
		-w :: enclosure width <integer/percent>  (default=60%/120)
		-i :: indent lines <integer>             (default=1 white space)
		-s :: side enclosure character           (default="##")
		-t :: top/bottom enclosure character     (default="#")
		-b :: bottom enclosure character <none>  (default=<top setting>)
		-B :: disable bottom enclosure line      (default=enabled)
	USAGE
}
function process_args(){
	COLUMNS=`tput cols`
	HEADER='false'
	INDENT='1'
	TOP="#"
	SIDE="##"
	UNDERLINE=${TOP}
	BOTTOM=${TOP}
	BOTTOM_LINE='true'
	WIDTH_DEFAULT_PERCENT='60'
	WIDTH_DEFAULT='120'
		
	while getopts "h?Hu:w:i:s:t:b:B" OPT; do
		case "${OPT}" in
			h|\?)	usage
				exit;;
			H)	HEADER='true';;
			u)	UNDERLINE=${OPTARG}
				HEADER='true';;
			w)	WIDTH=${OPTARG};;
			i)	INDENT=${OPTARG};;
			s)	SIDE=${OPTARG};;
			t)	TOP=${OPTARG};;
			b)	BOTTOM=${OPTARG};;
			B)	BOTTOM_LINE='false';;
		esac
	done

	WIDTH=$(get_width)
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
function main(){
	process_args "$@"
	# top line
	local TOP_LINE=$(seq ${WIDTH} | sed "s/.*/${TOP}/" | tr -d \\\n)
	local TOP_LINE=${TOP_LINE:0:${WIDTH}}
	local INDENT=$(seq ${INDENT} | sed "s/.*/ /" | tr -d \\\n)
	local WRAP_WIDTH=$(( WIDTH - ( 2 * ( ${#SIDE} ${#INDENT} ) ) ))
	echo "$TOP_LINE"
	# first line
	read line
	###
	#		echo "This is way to longfor es" | sed 's/\(.\{5\}\)/\1\n/' | sed '1!{s/\(.\{0,4\}\)/ \1\n/g};s/\n$//'
	###
	line="${SIDE}${INDENT}${line}"
	line=$(printf "%s%$(( WIDTH - ${#line} ))s" "${line}" "${INDENT}${SIDE}")
	(( ${#line} > WIDTH)) && WIDTH=${#line}
	echo "${line}"
	# underline
	if ${HEADER}; then
		local UNDERLINE=${SIDE}$(seq ${WIDTH} | sed "s/.*/${UNDERLINE}/" | tr -d \\\n)
		local UNDERLINE=${UNDERLINE:0:$(( WIDTH - ${#SIDE} ))}${SIDE}
		echo "${UNDERLINE}"
	fi
	# body
	while read line; do
		line="${SIDE}${INDENT}${line}"
		line=$(printf "%s%$(( WIDTH - ${#line} ))s" "${line}" "${INDENT}${SIDE}")
		(( ${#line} > WIDTH)) && WIDTH=${#line}
		echo "${line}"
	done
	# bottom line
	if ${BOTTOM_LINE}; then
		local BOTTOM_LINE=$(seq ${WIDTH} | sed "s/.*/${BOTTOM}/" | tr -d \\\n)
		local BOTTOM_LINE=${BOTTOM_LINE:0:${WIDTH}}
		echo "${BOTTOM_LINE}"
	fi
}
main "$@"
