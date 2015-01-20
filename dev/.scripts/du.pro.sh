#!/bin/bash
#	local   CLEAR='\033[K'
#	local    SAVE='\033[s'
#	local RESTORE='\033[u'
#	local     UP1='\033[1A'
#	local   DOWN1='\033[1B'
#	local NEWLINE='\n'
#	local     TAB='\t'

function prep_du(){
	du -ckhd100 "$@" 2>/dev/null
}
function human_du(){
	du -cksh "$@" 2>/dev/null
}
function bytes_du(){
	du -cks "$@" 2>/dev/null
}
function sort_du(){
	bytes_du "$@" | sort -n
}
function clear_last_x_lines(){
	local UP_CLEAR=( `seq ${1:-1}` )
	local    CLEAR='\033[K'
	local     SAVE='\033[s'
	local  RESTORE='\033[u'
	local      UP1='\033[1A'
	UP_CLEAR=${UP_CLEAR[@]/*/${UP1}${CLEAR}}
	UP_CLEAR=${UP_CLEAR// /}
	printf "${SAVE}${UP_CLEAR}${RESTORE}"
}
function move_up_x_lines(){
	printf '\033['${1:-1}'A'
}
function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-i :: invert numeric sort
		-l :: number of lines to scroll (default=5)
		-p :: skip prep
		-r :: number of results         (default=10)
	USAGE
}
function process_args(){
	SCROLL_LINES='5'
	MAX_RESULTS='10'
	SKIP_PREP=false
	NUMERIC_SORT='-n'
	while getopts "ah?il:pr:" OPT; do
		case "${OPT}" in
			h|\?)	usage
				exit;;
			a)	MAX_RESULTS='1000';;
			i)	NUMERIC_SORT='-nr';;
			l)	SCROLL_LINES="${OPTARG}";;
			p)	SKIP_PREP=true;;
			r)	MAX_RESULTS="${OPTARG}";;
			
		esac
	done
	(( SCROLL_LINES > MAX_RESULTS )) && MAX_RESULTS=${SCROLL_LINES}
	return $(( ${OPTIND} - 1 ))
}
function awk_script(){
	cat <<-AWK
		BEGIN {
			NEWLINE = "\n"
			TAB     = "\t"
			FORMAT  = "%-11s%-6s%s%s"
		}
		{	
			if (SORT) {
				SIZE = \$1
				\$1 = ""
				\$0 = \$0
				\$1 = \$1
				printf (FORMAT, SIZE, ARRAY[\$0], \$0, NEWLINE)
			} else {
				SIZE = \$1
				\$1 = ""
				\$0 = \$0
				\$1 = \$1
				ARRAY[\$0] = SIZE
				if (\$NF ~ "^total$") {
					SORT = "true"
				}
			}
		}
	AWK
}
function main(){
	# Local Variables
	local HUMAN="/run/shm/$$_HUMAN_${RANDOM}"
	local BYTES="/run/shm/$$_BYTES_${RANDOM}"
	local  SORT="/run/shm/$$_SORT_${RANDOM}"

	# process arguments
	process_args "$@"
	shift $?

	# if 1 input input argument and its a folder then compare child file/folders
	(( ${#@} == 1 )) && [ -d "${1}" ] && set -- "${1}"/{.[a-zA-Z]*,*}

	# PREP
	if ! ${SKIP_PREP}; then
		prep_du "$@" | ./scroll.sh -l ${SCROLL_LINES}
		clear_last_x_lines ${SCROLL_LINES}
		move_up_x_lines ${SCROLL_LINES}
	fi

	# RESULTS
	human_du "$@" | tee "${HUMAN}" | ./scroll.sh -l ${SCROLL_LINES}
	clear_last_x_lines                              ${SCROLL_LINES}
	move_up_x_lines                                 ${SCROLL_LINES}
	bytes_du "$@" | tee "${BYTES}" | ./scroll.sh -l ${SCROLL_LINES}
	clear_last_x_lines                              ${SCROLL_LINES}
	move_up_x_lines                                 ${SCROLL_LINES}
	
	# SORT
	cat "${BYTES}" | sort ${NUMERIC_SORT} > "${SORT}"

	# ASSOSIATION
	cat "${HUMAN}" "${SORT}" |
	awk -W interactive -f <(awk_script) |
	./scroll.sh -l ${MAX_RESULTS}

	# Clean Up
	rm "${HUMAN}"
	rm "${BYTES}"
	rm "${SORT}"
}
main "$@"
