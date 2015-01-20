#!/bin/bash
if true; then
	if ! ps -o comm | grep -q caffeinate; then
		nohup caffeinate -d -i -s -u -t 60 &>/dev/null &
	fi

	pmset schedule wake "$(date -v +7S '+%m/%d/%y %H:%M:%S')" &
else
	killall -9 caffeinate
	pmset sleepnow
fi
exit 0


