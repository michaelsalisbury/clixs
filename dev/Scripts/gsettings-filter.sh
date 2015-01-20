#!/bin/bash
source=http://10.173.119.78/scripts/system-setup/$scriptName
scriptName=$(basename  "$(readlink -f ${BASH_SOURCE})")
scriptPath=$(dirname   "$(readlink -f ${BASH_SOURCE})")
packagename=$(basename "${scriptPath}")
LOG="${HOME}/.logs/${scriptName}"

function main(){
	local -a lines=( )
	IFS=$'\n' read -d $'' -a lines < <(gsettings list-recursively)

	diff <(IFS=$'\n'; echo "${lines[*]}") <(gsettings list-recursively)


	return 0

	GSET_MONITOR

	return 0

	while read SCHEMA; do
		#GSET_SCHEMA_IS_PARENT ${SCHEMA}	&& echo parent
		#GSET_SCHEMA_IS_CHILD  ${SCHEMA}	&& echo child
		echo -n -e     $(GSET_SCHEMA_IS ${SCHEMA})
		echo -n -e '\t'$(GSET_KEY_COUNT ${SCHEMA})
		echo -n -e '\t'$(GSET_GET_PARENT_COUNT ${SCHEMA})
		echo    -e '\t'${SCHEMA}
	done < <(gsettings list-schemas)
}
function GSET_FIND_CHILD_PID(){
	local ppid=$1
	shift
	local child=""
	for child in $(GSET_GET_CHILD_PIDS ${ppid}); do
		ps --no-heading -o cmd -p ${child} |\
		grep "$@" |\
		sed "s/^/${child} /"
	done
}
function GSET_GET_CHILD_PIDS(){
	local ppid=$1
	local child=""
	ps --no-heading -o pid -p     ${ppid} &> /dev/null || { echo PID ${ppid} does not exist. 1>&2; return 1; }
	for child in $(ps --no-heading -o pid --ppid ${ppid}); do
		GSET_GET_CHILD_PIDS ${child}
		echo ${child}
	done
}
function GSET_MONITOR(){
	#local timeout=${1:-60}
	local timeout=${1:-10}
	local FORK=""
	local -a FORKS=( )
	local INDEX=0
	local SCHEMA=""
	while read SCHEMA; do
		echo MONITORING :: ${SCHEMA}
		GSET_MONITOR_PROC ${SCHEMA} &
		FORKS[(( INDEX++ ))]=$!
	done < <(gsettings list-schemas)
	sleep ${timeout}
	local pid=""
	local cmd=""
	for PROC in ${PROCS[*]}; do
		while read pid cmd; do
			echo ${cmd}
			kill -USR1 ${pid}
		done < <(GSET_FIND_CHILD_PID ${PROC} "gsettings monitor") &
	done
}
function GSET_MONITOR_PROC(){
	local SCHEMA=$1
	local KEY=""
	gsettings monitor ${SCHEMA} |\
		while read KEY; do
			echo "${SCHEMA} ${KEY}" >> "${LOG}"
		done
}

function GSET_GET_PARENTS(){
	local SCHEMA=$1
	GSET_SCHEMA_IS_CHILD ${SCHEMA} || return 1
	local PARENT=""
	local -a PARENTS=""
	local INDEX=0
	while read PARENT; do
		[[ "${SCHEMA}" =~ ^"${PARENT}".+ ]] && PARENTS[(( INDEX++ ))]=${PARENT}
	done < <(gsettings list-schemas)
	local IFS=$'\n'
	echo "${PARENTS[*]}"
}
function GSET_GET_PARENT_COUNT(){
	local SCHEMA=$1
	local PARENT_COUNT=$(GSET_GET_PARENTS ${SCHEMA} | wc -l)
	(( PARENT_COUNT > 0 )) && echo ${PARENT_COUNT}
}
function GSET_GET_PARENT(){
	local SCHEMA=$1
	GSET_GET_PARENTS ${SCHEMA} | sort | tail -1
}
function GSET_KEY_COUNT(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	local KEY_COUNT=$(gsettings list-keys ${SCHEMA} | wc -l)
	(( KEY_COUNT > 0 )) && echo ${KEY_COUNT}
}
function GSET_LIST_PARENTS(){
	local SCHEMA=""
	while read SCHEMA; do
		gsettings list-children ${SCHEMA} | grep -q "" || continue
		echo ${SCHEMA}
	done < <(gsettings list-schemas)
}
function GSET_LIST_PARENTS_WITH_KEYS(){
	local SCHEMA=""
	while read SCHEMA; do
		gsettings list-keys ${SCHEMA} | grep -q "" || continue
		echo ${SCHEMA}
	done < <(GSET_LIST_PARENTS)
}
function GSET_IS_SCHEMA(){
	local SCHEMA=$1
	gsettings list-children ${SCHEMA} >> /dev/null
}
function GSET_SCHEMA_IS_PARENT(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	local CHILD=""
	while read CHILD; do
		[[ "${CHILD}" =~ ^"${SCHEMA}".+ ]] && return 0
	done < <(gsettings list-schemas)
	return 1
}
function GSET_SCHEMA_IS_CHILD(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	local PARENT=""
	while read PARENT; do
		[[ "${SCHEMA}" =~ ^"${PARENT}".+ ]] && return 0
	done < <(gsettings list-schemas)
	return 1
}
function GSET_SCHEMA_IS_ORPHAN(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	! GSET_SCHEMA_IS_PARENT ${SCHEMA} &&\
	! GSET_SCHEMA_IS_CHILD  ${SCHEMA}
}
function GSET_SCHEMA_IS_BOTH(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	GSET_SCHEMA_IS_PARENT ${SCHEMA} &&\
	GSET_SCHEMA_IS_CHILD  ${SCHEMA}
}
function GSET_SCHEMA_HAS_KEYS(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	gsettings list-keys ${SCHEMA} | grep -q ""
}
function GSET_SCHEMA_IS(){
	local SCHEMA=$1
	GSET_IS_SCHEMA ${SCHEMA} || return 1
	GSET_SCHEMA_IS_PARENT ${SCHEMA} && local PARENT='PARENT'	
	GSET_SCHEMA_IS_CHILD  ${SCHEMA} && local CHILD='CHILD'
	if [ -n "${PARENT}" ] && [ -n "${CHILD}" ]; then
		echo BOTH
	elif [ -z "${PARENT}" ] && [ -z "${CHILD}" ]; then
		echo ORPHAN
	else	
		echo ${PARENT}${CHILD}
	fi
}





touch "${LOG}"
main "$@" 2>&1 | tee -a "${LOG}"
