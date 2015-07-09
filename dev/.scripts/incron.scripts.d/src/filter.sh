#!/bin/bash

function process_filters(){
	# The include filter must process true or not exist to then move on
	#   and process the exclude filters
	# The existence of fixed filters trump the regex filters
	if
		if [ -e "${INCLUDE_FIXED}" ]; then
			grep -q -x -F -f "${INCLUDE_FIXED}" <<< "${INCRON_MATCH}"
		elif [ -e "${INCLUDE_REGEX}" ]; then
			grep -q -x -f "${INCLUDE_REGEX}" <<< "${INCRON_MATCH}"
		fi
	then
		if [ -e "${EXCLUDE_FIXED}" ]; then
			grep -q -v -x -F -f "${EXCLUDE_FIXED}" <<< "${INCRON_MATCH}"
		elif [ -e "${EXCLUDE_REGEX}" ]; then
			grep -q -v -x -f "${EXCLUDE_REGEX}" <<< "${INCRON_MATCH}"
		fi
	else
		false
	fi
}
function process_args(){
	while [[ "${1//[^-]/.}" != +(--|) ]]; do
		case "${1}" in
			-v)	show_version; exit $?;;
			-t)	component_test; exit $?;;
			*)	false;;
		esac
		(( $? )) || { shift; continue; }
		
		${INCRON_ACTION_TEST[${1//[^[:alpha:]_]/.}]:-false} &&
			INCRON_ACTION=$1 &&
			shift && continue
		shift
	done
	shift
	export INCRON_MATCH=${INCRON_MATCH:-${*}}
	(( ${INCRON_ACTION+1} )) && export INCRON_ACTION

}
function list_action_app_names(){
	ls -1 "${WORK}" |
	sed -n "s/^${APP}@\(.*\)\.\(fixed\|regex\)\.include$/\1/p" |
	sort -u
}
function process_action(){
	INCLUDE_FIXED="${WORK}/${APP}@${ACTION}.fixed.include"
	INCLUDE_REGEX="${WORK}/${APP}@${ACTION}.regex.include"
	EXCLUDE_FIXED="${WORK}/${APP}@${ACTION}.fixed.exclude"
	EXCLUDE_REGEX="${WORK}/${APP}@${ACTION}.regex.exclude"
	ACTION="${WORK}/${ACTION}.${EXT}"
	process_filters || return 1
	"${ACTION}" "$@" &
}
function process_actions(){
	list_action_app_names |
	while read ACTION; do
		process_action "$@"
	done
}
function main(){
	touch "${INCLUDE_REGEX}"
	process_args "$@"
	process_filters || return 1
	process_actions "$@"
}
function component_test(){
	show_version
}
function show_version(){
	mk_version "/etc/incron.scripts.d/src/scripts.common.sh"
	mk_version "${EXEC}"
}
. /etc/incron.scripts.d/src/scripts.common.sh
set_global_vars
set_INCRON_ACTION_TEST
### global vars
# Filters :: includes are processed before excludes
#            fixed filters trump regex filters
INCLUDE_FIXED="${WORK}/${APP}.fixed.include"
INCLUDE_REGEX="${WORK}/${APP}.regex.include"
EXCLUDE_FIXED="${WORK}/${APP}.fixed.exclude"
EXCLUDE_REGEX="${WORK}/${APP}.regex.exclude"

exec {fd_LOG}> >(tee -a "${LOG}")
main "$@" >& ${fd_LOG}
