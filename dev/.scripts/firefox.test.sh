#!/bin/bash
. ${BASH_SOURCE%/*}/firefox.common.sh

profile="apps"
site=$1
shift
filter=$*

function is_window_open(){
	is_profile_open || return 1
	cat <<-AWK | awk -vP="^${profile}$" -vF="${filter}" -f <(cat) <(wmctrl -l) |
		{ split(\$0,TITLE,\$4" : ") }
		\$4 ~ P && \$5 == ":" && TITLE[2] ~ F { print "true" }
	AWK
	grep -q ""
}
function launch(){
	#firefox -P ${profile} -new-instance -new-window ${site} &
	firefox -P ${profile} -new-window ${site} &
}

# Find open window
if ! profile_exists; then
	echo No profile with name \"${profile}\". Exiting\! 1>&2
	exit 1
elif is_window_open; then
	# if open bring to front
	#switch_to_window
	
elif ! (( ${#site} )); then
	echo No site name provided. Exiting\! 1>^&2
	exit 1
else
	# if not open launch
	launch
fi

exit

