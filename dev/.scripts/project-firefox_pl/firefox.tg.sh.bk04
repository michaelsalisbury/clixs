#!/bin/bash
. ${BASH_SOURCE%/*}/firefox.common.sh

profile=$1
shift
filter=$*

function launch(){
	firefox -new-instance -P ${profile} -new-window &
}

# Find open window
if ! profile_exists; then
	echo No profile with name \"${profile}\". Exiting\! 1>&2
	exit 1
elif is_profile_open; then
	# if open bring all instances to front
	switch_to_window
else
	# if not open launch
	launch
fi
