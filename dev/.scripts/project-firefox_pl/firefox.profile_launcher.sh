#!/bin/bash
. ${BASH_SOURCE%/*}/firefox-common.sh
# GLOBAL VARS: dependencies, profile, site, filter
# Firefox Dependencies: to find a firefox window using a particular profile the "Show Profile" extention must be installed
dependencies="notify-send gawk grep egrep wmctrl curl"
profile="apps"
site="google.com"
# ENVIRONMENT VARS:
#	FIREFOX_TARGET_PROFILE 
#	FIREFOX_DEFAULT_PROFILE
#	FIREFOX_TARGET_SITE
#	FIREFOX_DEFAULT_SITE

function process_args(){
	# determine firefox profile to use
	if firefox_profile_exists ${1//[\$\`]/}; then
		profile=${1//[\$\`]/}
		shift
	elif firefox_profile_exists ${FIREFOX_TARGET_PROFILE:-${FIREFOX_DEFAULT_PROFILE:-${profile}}}; then 
		profile="${FIREFOX_TARGET_PROFILE:-${FIREFOX_DEFAULT_PROFILE:-${profile}}}"
	else
		# throw error about how a valid profile was not supplied
		echo 1 :: ERROR :: Invalid profile provided\; \"${1//[\$\`]/}\" \"${FIREFOX_TARGET_PROFILE:-${FIREFOX_DEFAULT_PROFILE:-${profile}}}\" 1>&2
		return 1
	fi
	# determine site to use if a firefox window is not already open
	if egrep -q -i '^none$' <<< "${1//[\$\`]/}" || url_is_valid 0.5 ${1//[\$\`]/}  ; then
		site=${1//[\$\`]/}
		shift
	elif egrep -q -i '^none$' <<< ${FIREFOX_TARGET_SITE:-${FIREFOX_DEFAULT_SITE:-${site}}} ||
		 url_is_valid 0.2 ${FIREFOX_TARGET_SITE:-${FIREFOX_DEFAULT_SITE:-${site}}}; then
		site=${FIREFOX_TARGET_SITE:-${FIREFOX_DEFAULT_SITE:-${site}}}
	else
		# throw error about how a valid url was not supplied or that "none" is also valid but must be specified
		echo 2 :: ERROR :: Invalid url provided\; \"${1//[\$\`]/}\" \"${FIREFOX_TARGET_SITE:-${FIREFOX_DEFAULT_SITE:-${site}}}\" 1>&2
		return 2
	fi
	# determine search filter to ID open firefox window
	filter=$*
	return 0
}
function launch_new_instance(){
	local site=$(egrep -v -i '^none$' <<< "${site}")
	firefox -P ${profile} -new-instance -new-window ${site} &>/dev/null &
}
function launch_new_window(){
	# The firefox command must be started up in a terminal to utilize the "-remote" argument
	local site=$(sed 's/^http[s]\{0,1\}:\/\///I' <<< "${site}")
	gnome-terminal --geometry 1x1+1+1 -e "firefox -P ${profile} -remote \"openUrl(http://${site}, new-window)\""
}
function main(){
	if ! process_args "$@"; then
		echo ERROR :: Invalid or missing arguments :: $? :: "$@" 1>&2
		EXIT 1
	fi
	if ! script_dependency_check ${dependencies}; then
		echo ERROR :: Missing dependencies :: "${dependencies}" 1>&2	
		EXIT 2
	fi
	global_var_status

	if is_window_open; then
		# if open bring to front
		echo Switching to open window.
		switch_to_window
	elif is_profile_open; then
		# if site is not open but other sites are running with the target profile then launch new window
		echo Launching new window.
		launch_new_window
	else
		# if not open launch new instance
		echo Launching new instance.
		launch_new_instance
	fi
}
function EXIT(){
	# restore stdout and stderr
	exec 1>&8 8>&-
	exec 2>&9 9>&-
	# a sleep command is required here to ensure that the sub-shells that redirect stdout and stderr
	# to logs close before exit is called
	# what is strange is that it seems that it can be quite small
	# if this line is omitted the script exits but the command prompt is not returned correctly
	sleep 0.00001
	echo Logs located at\; ${LOG}.std{err,out}
	exit ${1:-0}
}
# prep logs
[ -d "${HOME}/.logs" ] || mkdir -p "${HOME}/.logs"
LOG="${HOME}/.logs/$(basename "${BASH_SOURCE}")"
! ls "${LOG}".std{err,out} &>/dev/null && touch "${LOG}".std{err,out}
# write log section entry header
echo `date` :: ----------------------------------------------------------- | tee -a "${LOG}".std{err,out} >/dev/null
# save stdout and stderr
exec 8>&1
exec 9>&2
# redirect stdout and stderr
exec  > >(tee -a "${LOG}.stdout")
exec 2> >(tee -a "${LOG}.stderr" 1>&2)
main "$@"
EXIT 0
