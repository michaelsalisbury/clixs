#!/bin/bash/

_vboxmanage_options_exp(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local _AWK _SED _line
	while read _line; do
		case "${_line}" in
			\<uuid\|vmname\>)	< <(vboxmanage list vms) cut -f2 -d\";;
			\<uuid\|vmname\>...)	< <(vboxmanage list vms) cut -f2 -d\";;
			#\<uuid\|filename\>)	;;
			#\<uuid\|inputfile\>)	;;
			#*)			echo "${OPTION}";;
			*)			false;;
		esac
		(( $? )) || continue
		# strip leading or trailing square brackets
		(( ${_AWK+1} )) || read -r -d $'' _AWK <<-AWK
			#/^\[/{
				gsub(/(^\[|\]$)/,"",\$0)
				#print \$0
				#next
			#}
			match(\$0,/<1-[N4]>$/){
				#for (i=1;i<=2;i++)
				\$0=substr(\$0,0,RSTART-1)
				print \$0 1
				print \$0 2
				next
			}	
			{print}
		AWK
		# replace pipes with cariage returns except for cases like <uuid|vmname>
		(( ${_SED+1} )) || read -r -d $'' _SED <<-SED
			s/\(<[a-zA-Z]\+\)|\([a-zA-Z]\+\)/\1@\2/g
			s/|/\n/g
			s/@/|/g
		SED
		<<< "${_line}" awk "${_AWK}" > ${_SHM}
		< ${_SHM}       sed "${_SED}"
		#< <(awk "${_AWK}" <<< "${_line}") sed "${_SED}"
	done
}
_COMPREPLY_put(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local WORD_DEPTH=${1:-1}
	case "${WORD_DEPTH}" in
		1)	#_COMPREPLY_put_list    < <(_vboxmanage_help_args)
			#_COMPREPLY_put_list -a < <(_vboxmanage_options_L1);;
			_vboxmanage_options_L1 > ${_SHM}
			< ${_SHM} _COMPREPLY_put_list;;

		[2-6])	#_COMPREPLY_put_list    < <(_vboxmanage_help_args)
			#_COMPREPLY_put_list -a < <(_vboxmanage_options_L${WORD_DEPTH} | _vboxmanage_options_exp);;
			_vboxmanage_options_L${WORD_DEPTH} > ${_SHM}
			< ${_SHM}  _vboxmanage_options_exp > ${_SHM}$
			< ${_SHM}$ _COMPREPLY_put_list;;
		[7-9]*)	#_COMPREPLY_put_list    < <(_vboxmanage_help_args)
			#_COMPREPLY_put_list -a < <(_vboxmanage_options_DEFAULT);;
			_COMPREPLY_put_list    < <(_vboxmanage_options_DEFAULT);;
	esac
}
_vboxmanage_select(){
	_vboxmanage_log "${@}"
	# this command returns true if the WORD at WORD_DEPTH (arg #1) is not in contention (parital or no match)
	local RETURN_ERR=
	local WORD_DEPTH=${1:-1}
	case "${WORD_DEPTH}" in
		L[0-9]*|l[0-9]*)	WORD_DEPTH=${WORD_DEPTH//[a-zA-Z]/};;
		[1-9]*)			;;
		*)			WORD_DEPTH=1;;
	esac
	local WORD="${COMP_WORDS[WORD_DEPTH]}"
	# if WORD_DEPTH (arg #1) is not in contention (equal to COMP_CWORD) then return true
	(( WORD_DEPTH < COMP_CWORD )) && return 0
	# WORD_DEPTH (arg #1) is in contention, get list of posible entries
	_COMPREPLY_put "${WORD_DEPTH}"
	# if no partial command has been typed then return list 
	! (( ${PARTIAL:+1} )) && _COMPREPLY && return 1
	# "PARTIAL" exists so filter
	_COMPREPLY "${PARTIAL}"
	return 1
}
_func_data_stor(){
	local _ref=${FUNCNAME[1]}
	if ! (( $# )); then
		(( ${!_ref+1} )) &&
			echo "${!_ref}" ||
			return 1	
	elif [ "${1:0:1}" == "-" ]; then
		eval ${_ref}=\$\(cat\)
		printf %s "${!_ref}"
	else
		eval ${_ref}=\"\$@\"
		printf %s "${!_ref}"
	fi
}
_vboxmanage_options_L1(){
	_vboxmanage_log "$@"
	_func_data_stor && return
	local _SHM=${SHM}${FUNCNAME}
	local _AWK
	read -r -d $'' _AWK <<-AWK
		NR==1,/^Commands:/{s=1;next}
		s-->0{next}
		!RSTART{match(\$0,\$1)}
		{\$0=substr(\$0,RSTART)}
		/^( |$)/{next}
		CMD!=\$1{print \$1}
		#{CMD=\$1}
	AWK
	vboxmanage               > ${_SHM}
	< ${_SHM}  awk "${_AWK}" > ${_SHM}$
	< ${_SHM}$ _func_data_stor -
}
_vboxmanage_options_L2(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	(( ${#COMMAND} )) || return
	local _AWK
	read -r -d $'' _AWK <<-AWK
		GET_OPT{GET_OPT=\$1}
		/^VBox/{GET_OPT=\$3}
		GET_OPT{
			# strip trailing pipe
			sub(/\|$/,"",GET_OPT)
			print GET_OPT
			# if the line ends with a pipe get first word of next line
			if(!/\|$/){GET_OPT=null}
		}
	AWK
	_vboxmanage_command ${COMMAND} > ${_SHM}
	< ${_SHM}  awk "${_AWK}"       > ${_SHM}$
	< ${_SHM}$ uniq
}
_vboxmanage_options_L3(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	local LAST_WORD_DEPTH=$(( COMP_CWORD - 1 ))
	local LAST=${COMP_WORDS[LAST_WORD_DEPTH]}
	(( ${#COMMAND} )) || return
	local _AWK1 _AWK2
	read -r -d $'' _AWK1 <<-AWK
		/^VBox/ && NF<${COMP_CWORD}+1 {exit 1}
	AWK
	read -r -d $'' _AWK2 <<-AWK
		BEGIN{
			MATCH="${LAST}"
			sub(/[0-9]+$/,"<",MATCH)
			MATCH="[ \\\|\\\[]" MATCH "[ \\\|\\\]1]"
		}
		match(\$0,MATCH){exit 1}
	AWK
	_vboxmanage_command ${COMMAND} > ${_SHM}
	if ! < ${_SHM} awk "${_AWK2}"; then
	   	_vboxmanage_options_L3A
	elif ! < ${_SHM} awk "${_AWK1}"; then
	   	_vboxmanage_options_L3C
	else
		_vboxmanage_options_L3D
	fi
}
_vboxmanage_options_L3A(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	local LAST_WORD_DEPTH=$(( COMP_CWORD - 1 ))
	local LAST=${COMP_WORDS[LAST_WORD_DEPTH]}
	local _AWK
	read -r -d $'' _AWK <<-AWK
		BEGIN{
			MATCH="${LAST}"
			sub(/[0-9]+$/,"<",MATCH)
			MATCH="[ \\\|\\\[]" MATCH "(\\\]|[\\\|1][^ ]+)? "
		}
		match(\$0,MATCH){
			\$0=substr(\$0,RSTART+RLENGTH)
			match(\$0,/^<[^>]+>/) ||
			match(\$1,/.*/)
			print substr(\$0,RSTART,RLENGTH)
		}
		END{print "TESTING_L3A"}
	AWK
	_vboxmanage_command ${COMMAND} > ${_SHM}
	< ${_SHM} awk "${_AWK}"
}
_vboxmanage_options_L3C(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	local LAST_WORD_DEPTH=$(( COMP_CWORD - 1 ))
	local LAST=${COMP_WORDS[LAST_WORD_DEPTH]}
	# returns the the first word of lines excluding /^VBox/
	local _AWK
	read -r -d $'' _AWK <<-AWK
		/^ *$/{GET_OPT=null; next}
		/^VBox/{GET_OPT=1; next}
		GET_OPT{
			GET_OPT=\$1
			# strip trailing pipe
			sub(/\|$/,"",GET_OPT)
			print GET_OPT
			# if the line ends with a pipe get first word of next line
			if(!(/\|$/||(\$1~/^\[/&&/\]$/))){GET_OPT=null}
		}
		END{print "TESTING_L3C"}
	AWK
	_vboxmanage_command ${COMMAND} > ${_SHM}
	< ${_SHM} awk "${_AWK}"
}
_vboxmanage_options_L3D(){
	_vboxmanage_log "$@"
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	local LAST_WORD_DEPTH=$(( COMP_CWORD - 1 ))
	local LAST=${COMP_WORDS[LAST_WORD_DEPTH]}
	# changes command help format
	local _AWK1 _AWK2
	read -r -d $'' _AWK1 <<-AWK
		/^VBox/{
			print \$1, \$2
			LIST=LIST \$4 " "
			match(\$3,/^[^\|]+/)
			match(\$0,substr(\$3,RSTART,RLENGTH))
		}
		{print substr(\$0,RSTART)}
		END{
			print LIST
		}
	AWK
	read -r -d $'' _AWK2 <<-AWK
		\$1=="${LAST}"{print \$2}
		END{
			gsub(/ /,"\n",\$0)
			printf \$0
			print "TESTING_ELSE"
		}
	AWK
	_vboxmanage_command ${COMMAND} > ${_SHM}
	< ${_SHM}  awk "${_AWK1}" > ${_SHM}$
	< ${_SHM}$ awk "${_AWK2}"
}
_vboxmanage_options_L4(){ _vboxmanage_options_L3;}
_vboxmanage_options_L5(){ _vboxmanage_options_L3;}
_vboxmanage_options_L6(){ _vboxmanage_options_L3;}
_vboxmanage_options_L7(){ _vboxmanage_options_L3;}

_vboxmanage_options_DEFAULT(){
	_vboxmanage_log "$@"
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	(( ${#COMMAND} )) || return
	exec 3<<-AWK
		END{print "TESTING"}
	AWK
	_vboxmanage_command ${COMMAND} \
	|awk -f <(cat <&3 3<&-)
}
_vboxmanage_help_args(){
	_vboxmanage_log "$@"
	echo ${PREPEND} | tr \  \\n
}
_vboxmanage_need_help(){
	_vboxmanage_log "$@"
	case "${COMP_WORDS[${COMP_CWORD}]:-${COMP_WORDS[COMP_CWORD-1]}}" in
		-h|--help|help)	return 0;;		
		*)		return 1;;
	esac
}
_vboxmanage_get_help(){
	_vboxmanage_log "$@"
	if _vboxmanage_need_help; then
		echo 1>&2
		echo HELP >> "${LOG_OUT}"
		_vboxmanage_command | tee -a "${LOG_OUT}" 1>&2
		return 0
	else
		return 1
	fi
}
_COMPREPLY_put_list(){
	_vboxmanage_log "$@"
	# expected use: _COMPREPLY_put_list [-a] < <(input)
	# if arg 1 is -a then append otherwise clear _COMPREPLY
	[ "$1" == "-a" ] || _COMPREPLY=()
	local _line
	while read _line; do _COMPREPLY[${#_COMPREPLY[*]}]=${_line}; done
}
_vboxmanage_command_all(){
	_vboxmanage_log "$@"
	while read COMMAND; do
	       	_vboxmanage_command ${COMMAND}
	done < <(< <(_vboxmanage_options_L1) uniq)
}
_vboxmanage_command(){
	_vboxmanage_log "$@"
	local _SHM=${SHM}${FUNCNAME}
	local COMMAND_WORD_DEPTH='1'
	local COMMAND="${1:-${COMP_WORDS[COMMAND_WORD_DEPTH]}}"
	(( ${#COMMAND} )) || return
	local _AWK1 _AWK2 _AWK3 _AWK4
	# 1; Skip upto /^Usage:/ plus one line (effects all commands). Skip /^Syntax error:/ (effects vboxmanage convertfromraw).
	# 2; Ammends commands who's subcommands have line depreciated attributes (effects vboxmanage guestcontrol).
	# 3; Rectify vboxmanage list 2nd argument [--long|-l] and join it to the 3rd argument
	# 4; Remove first instance of vboxmanage guestcontrol since it will have been replicated by the 2nd step above.
	# 5; A number of inconsistencies in vboxmanage modifyvm
	read -r -d $'' _AWK1 <<-AWK
		############################################################################ 1
		NR==1,/^Usage:/{s=1;next}
		s-->0{next}
		/^Syntax error:/{next}
		############################################################################ 2
		/^VBoxManage/{CMD_LINE=\$0}
		CMD_LINE_NEXT && !/^ *$/ && !/^VBoxManage/{
			match(\$0,/^ */)
			\$0=CMD_LINE" "substr(\$0,RLENGTH)
		}
		{CMD_LINE_NEXT=/^ *$/}
		############################################################################ 3
		"list"=="${COMMAND}" && "${COMMAND}"==\$2{
			match(\$3,/[^\[\]]+/)
			\$4=substr(\$3,RSTART,RLENGTH)"|"\$4
			gsub(/./," ",\$3)
		}
		############################################################################ 4
		"guestcontrol"=="${COMMAND}" && /^VBoxManage/ && !ONCE {ONCE=1;next}
		############################################################################ 5
		/--nicgenericdrv</	|| /--natbindip</	||
		/--keyboard /		|| /--draganddrop /	||
		/--vrdeextpack /	|| /--vrdeauthlibrary /	||
		/--teleporteraddress /	|| /--vcpscreens /	||
		/--diffparent /					{\$0=\$0"]"}
		sub(/<ipv[46]>/," &",\$0)
		sub(/<length>/," &",\$0)
		############################################################################ 6
		"hostonlyif"=="${COMMAND}" && "${COMMAND}"==\$2{
			sub(/ ipconfig /," ipconfig|create|remove ",\$0)
		}
		{print}
	AWK
	# joins indented lines to their parent
	# ERROR :: BUGFIX :: don't add space if last line ended with a pipe
	read -r -d $'' _AWK2 <<-AWK
		/^ *\$/{printf "\n";next}
		/^VBoxManage/{
			printf NR==1 ? \$0 : "\n"\$0
			match(\$3,/^.*[^\|]/)
			match(\$0,substr(\$3,0,RLENGTH))
			INDENT=RSTART
			next
		}
		{
			match(\$0,/[^[:space:]]/)
			#bug fix this
			printf RSTART<=INDENT ? "\n"\$0 : (!PIPE?" ":"")substr(\$0,RSTART)
			if(/\|$/){PIPE=1}else{PIPE=null}
		}
	AWK
	# expands arguments like exec[ute] to execute
	# remove default comments; anything in parentheses
		local END="[ \|\[\]]"
		local ALPHA="[a-zA-Z]"
	read -r -d $'' _AWK3 <<-AWK
		# expand arguments
		match(\$0,/${END}${ALPHA}+\[${ALPHA}+\](${END}|$)/)||
		match(\$0,/${END}\[${ALPHA}+\]${ALPHA}+(${END}|$)/){
			MATCH=substr(\$0,RSTART,RLENGTH)
			gsub(/[\[\]]/,"",MATCH)
			\$0=substr(\$0,0,RSTART-1) MATCH substr(\$0,RSTART+RLENGTH)
		}
		# remove defaults
		match(\$0,/ \([^\(\)]+\)$/){
			\$0=substr(\$0,0,RSTART-1)
		}
		{print}
	AWK
	# limits output to the correct sub command
	# effects: showvminfo,convertfromraw,usbfilter,sharedfolder,guestcontrol,guestproperty,metrics,natnetwork,dhcpserver
	read -r -d $'' _AWK4 <<-AWK
		3>${COMP_CWORD}{print;MATCH=1;next}
		{LINES[LENGTH++]=\$0}			
		(/^VBox/ && \$3~/(^|\|)${COMP_WORDS[2]}(\||\$)/),/^ *$/ {print;MATCH=1;next}
		(/^VBox/ && \$4~/(^|\|)${COMP_WORDS[3]}(\||\$)/),/^ *$/ {print;MATCH=1;}
		END{
			if (!MATCH)
				for (INDEX=0;INDEX<LENGTH;INDEX++)
					print LINES[INDEX]
		}
	AWK
	#################################################################################
	vboxmanage ${COMMAND}     > ${_SHM}
	< ${_SHM}  awk "${_AWK1}" > ${_SHM}$
	< ${_SHM}$ awk "${_AWK2}" > ${_SHM}
	< ${_SHM}  awk "${_AWK3}" > ${_SHM}$
	< ${_SHM}$ awk "${_AWK4}"
}
_vboxmanage() {
    LOG_OUT="/dev/shm/vboxmanage_out"
    LOG_BUG="/dev/shm/vboxmanage_bug"
    #SHM="/dev/shm/vboxmanage_$$"
    SHM="/dev/shm/vboxmanage_${RANDOM}"
    # array "COMP_WORDS" holds all the command line arguments
    # var   "COMP_CWORD" holds the length of array "COMP_WORDS"
    #                    it is also the word in contention
    if (( ${3+1} )); then
	local PARTIAL=$2
	local CURRENT=$3
	local PREVIOS=${COMP_WORDS[COMP_CWORD-2]}
    else
	local PARTIAL=
	local CURRENT=$2
	local PREVIOS=${COMP_WORDS[COMP_CWORD-1]}
    fi
    local PROGRAM=$1
    local COMMAND=$2
    COMPREPLY=()

    local START=
    _vboxmanage_log '###########################################################'
    _vboxmanage_log -s "$@"
    _vboxmanage_main "$@"
    { rm -vf "${SHM}"* >> ${LOG_OUT} & disown; } &>/dev/null
    _vboxmanage_log -l
    return 0
}
_vboxmanage_main(){
    #_vboxmanage_log ${CddOMP_CWORD} "$(declare -p COMP_WORDS)"
    _vboxmanage_log ${CddOMP_CWORD}
    local _SHM=${SHM}${FUNCNAME}

    PREPEND="-h --help help"
    OPTIONAL=":opts_optional:"

    _vboxmanage_select command || return 0

    _vboxmanage_need_help && _vboxmanage_get_help && return 0

    _vboxmanage_select L2 || return 0

	local _AWK
	read -r -d $'' _AWK <<-AWK
		/^VBox/{
			print "[COMMAND]", \$1, \$2
			match(\$3,/^[^\|]+/)
			match(\$0,substr(\$3,RSTART,RLENGTH))
		}
		{print "  "substr(\$0,RSTART)}
	AWK
	#_vboxmanage_command      > ${_SHM}
	#< ${_SHM} awk "${_AWK}" >> ${LOG_OUT}

	echo [OPTIONS] >> "${LOG_OUT}"
	#_vboxmanage_options_L3 >> "${LOG_OUT}"
	#_vboxmanage_command | grep -v ]$ >> "${LOG_OUT}"
	#_vboxmanage_command_all | awk '/^VBox/ && $4 {print}' >> "${LOG_OUT}"
    
	#_vboxmanage_command_all | grep "^ *\[.*[^]]$" >> "${LOG_OUT}"
	#_vboxmanage_command_all        > ${_SHM}
	#< ${_SHM} grep "^ *\[.*[^]]$" >> ${LOG_OUT}

	echo [PARSE] >> "${LOG_OUT}"

    _vboxmanage_select L3 || return 0

    _vboxmanage_select L${COMP_CWORD} || return 0

}
################################################################################
################################################################################
############################################################## supplemental
function flushfd_to_line_array(){
	readfd "$@"
}
function flushfd(){
	readfd "$@"
}
function readfd(){
	local _name _fd IFS=$''
	(( $# )) || set -- 3
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		if [ -e "/proc/${BASHPID}/fd/${_fd}" ]; then
			echo    -n $'\255'  >&${_fd}
			# the "-r" argument in the following read command preserves escaped characters
			read -r -d $'\255' -u ${_fd} REPLY[${_fd}]
			case "${FUNCNAME[1]}" in
				flushfd)        	;;
				flushfd_to_line_array)	IFS=$'\n'
							read -r -a REPLY_${_fd} <<< "${REPLY[${_fd}]}";;
				*)              	printf %s "${REPLY[${_fd}]}";;
			esac
		else
			unset REPLY[${_fd}]
		fi
	done
	(( ${#REPLY[${_fd}]} ))
}
function teefd(){
	local _fd _name _sfd=${1:-3}
	case "${FUNCNAME[1]}" in
		clonefd)	flushfd ${_sfd};;
		*)		readfd ${_sfd};;
	esac
	shift
	(( $# )) || set -- ${_sfd}
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		[ -e "/proc/${BASHPID}/fd/${_fd}" ] || eval "exec ${_fd}<><(:)"
		printf %s "${REPLY[${_sfd}]}" >&${_fd}
	done
	(( ${#REPLY[${_sfd}]} ))
}																					
function randomfd(){
	local _fd _loop='3'
	for _fd in $*; do
		while (( _loop-- )); do
			eval "exec {${_fd}}<><(:)" 2>/dev/null && break
		done
		unset REPLY[${_fd}]
	done
}
_vboxmanage_log(){
	local LAST_STAMP=${TIME_STAMP:-0}
	TIME_STAMP=$(date +%s.%N)
	case "$1" in
		-s)	shift
			log_START=${TIME_STAMP}
			TIME_START=$(date --date="@${log_START}" +%H:%M:%S.%N)
			log_PAD="${TIME_START//?/ }"
			echo ${TIME_START} ${BASHPID} ${FUNCNAME[1]} [START] :: "$@";;
		-l)	shift
			log_LAPSE=$(<<< "${TIME_STAMP} - ${log_START}" bc)
			log_LAPSE=${log_PAD%${log_LAPSE//?/?}}${log_LAPSE}
			echo "${log_LAPSE}" ${BASHPID} ${FUNCNAME[1]} [LAPSE] :: "$@";;
		*)	log_LAPSE=$(<<< "${TIME_STAMP} - ${LAST_STAMP}" bc)
			log_LAPSE=${log_PAD%${log_LAPSE//?/?}}${log_LAPSE}
			echo "${log_LAPSE}" ${BASHPID} ${FUNCNAME[1]} "$@";;
	esac >> "${LOG_BUG}"
}
_COMPREPLY(){
    _vboxmanage_log "$@"
    local _SHM=${SHM}${FUNCNAME}
    # expects _COMPREPLY to already be populated with
    #   un-escaped posible matches with or without spaces
    # filters COMPREPLY against ${COMP_WORDS[COMP_CWORD]}
    # handles matches with spaces
    # return true if any number of matches are found and entered into array "COMPREPLY"
    # returns -1 if array "_COMPREPLY" was un populated with responces to test
    # returns -2 if an exact match was found, the array "COMPREPLY" should have one entry
    unset COMPREPLY
    (( ${#_COMPREPLY[*]} )) || return 1
    local REGEX=${1:-${COMP_WORDS[COMP_CWORD]}}
    local MATCH

    printf "%q\n" "${_COMPREPLY[@]}" > ${_SHM}
    < ${_SHM} sort -r > ${_SHM}$

    while read MATCH; do
	[[ "${MATCH}" =~ ^[\[\<] ]] ||	
	MATCH=$(printf "%q" "${MATCH}")
	(( ${REGEX:+1} )) && ! [[ "${MATCH}" =~ ^"${REGEX}" ]] && continue
	COMPREPLY[${#COMPREPLY[*]}]=${MATCH}
	[[ "${MATCH}" =~ ^"${REGEX}"$ ]] && return 2
    done < ${_SHM}$
    #done < <(printf "%q\n" "${_COMPREPLY[@]}" | sort -r)
    return 0
}
################################################################################
################################################################################
############################################################## complete
complete -F _vboxmanage vboxmanage
