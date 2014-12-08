#!/bin/bash
function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-l :: number of lines to scroll          (default=5)
		-s :: delay line scroll by seconds       (>0.1)
		-d :: disable scroll delay after # lines (default=20)
	USAGE
}
function process_args(){
	MAX_LINES='5'
	SCROLL_DELAY='0'
	DELAY_DISABLE='20'
	while getopts "h?d:l:s:" OPT; do
		case "${OPT}" in
			h|\?)	usage
					exit;;
			d)		DELAY_DISABLE="${OPTARG}";;
			l)		MAX_LINES="${OPTARG}";;
			s)		SCROLL_DELAY="${OPTARG}";;
		esac
	done
}
function awk_script(){
	cat <<-AWK
		BEGIN {
			MAX_LINES = ${MAX_LINES}
			SCROLL_DELAY = ${SCROLL_DELAY}
			DELAY_DISABLE = ${DELAY_DISABLE}
			CLEAR   = "\033[K"
			SAVE    = "\033[s"
			RESTORE = "\033[u"
			UP1     = "\033[1A"
			DOWN1   = "\033[1B"
			NEWLINE = "\n"
			TAB     = "\t"
			
		}
		{
			INDEX += 1				# incrament INDEX
			ARRAY[INDEX] = \$0		# save next line to ARRAY
			
			if (SCROLL) {
					INDEX -= MAX_LINES		# decrement INDEX to the line to be removed from memory
					ARRAY[INDEX] = ""		# wipe old line from ARRAY

					for (LINE = MAX_LINES; LINE != 0; LINE--) {
						INDEX += 1
						printf ("%s%s%s%s%s%s%s", RESTORE, CLEAR, "\033["LINE"A", CLEAR, INDEX, TAB, ARRAY[INDEX])
					}
			} else {
					printf ("%s%s%s%s%s%s%s%s", SAVE, INDEX, TAB, ARRAY[INDEX], NEWLINE, RESTORE, DOWN1, CLEAR)

					if (INDEX >= MAX_LINES) {
						printf ("%s%s%s", NEWLINE, UP1, SAVE)
						SCROLL = "SCROLL"
					}
			}

			if (SCROLL_DELAY) {
				system("sleep "SCROLL_DELAY)
				if (INDEX > DELAY_DISABLE) SCROLL_DELAY = ""
			}
		}
		END {
			printf "\n"
		}
	AWK
}
function main(){
	process_args "$@"
	awk -f <(awk_script)
}
main "$@"
