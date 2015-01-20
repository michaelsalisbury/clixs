#!/bin/bash

# kill "System Events" process before apple script launch
#killall "System Events"

cat <<-SUSPEND > /tmp/suspend.bash.sh
	sleep 3
	"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" -suspend
	touch /tmp/suspend.success
SUSPEND
cat <<-APPLESCRIPT > /tmp/suspend.applescript.aps
	try
		tell application "System Events"
			-- escape key
			key code 53
			key code 53
			delay 1
			-- space bar
			key code 49 using command down
			delay 1
			-- type
			keystroke "Terminal"
			delay 1
			keystroke return
			delay 1
			keystroke "nohup /bin/bash /tmp/suspend.bash.sh &"
			keystroke return
			delay 1
			-- exit command line
			keystroke "exit 0"
			keystroke return
			delay 1
			-- quit window
			keystroke "q" using command down
		end tell
	end try
APPLESCRIPT

# launch applescript for all users: try block will prevent hang
while read username; do
		#echo osascript /tmp/suspend.applescript.aps
	{
		echo osascript /tmp/suspend.applescript.aps \&\>/dev/null |\
		su - ${username}
	} &
	PIDS=( $! ${PIDS[*]} )
done < <(who | awk '/console/{print $1}')

# wait for scripts to finish
while true; do
	LAPSE=$(ps -o etime -p $$ | tail -1 | cut -d: -f2 | sed 's/^0*//')
	RUNNING=$(ps -p ${PIDS[*]} | tail -n +2 | wc -l)
	if (( RUNNING == 0 )); then
		echo all child processes finish successfully
		ERROR=0
		break
	elif [ -e "/tmp/suspend.success" ]; then
		echo killing remaining child processes :: logout successfull
		kill ${PIDS[*]} &> /dev/null
		killall "System Events"
		ERROR=0
		echo ${LAPSE}
		break
	elif (( RUNNING == ${#PIDS[*]} )) && (( ${LAPSE:-0} > 7 )); then
		echo killing all child processes :: no users logged in
		kill ${PIDS[*]} &> /dev/null
		killall "System Events"
		ERROR=1
		break
	elif (( ${LAPSE:-0} > 10 )); then
		echo killing remaining child processes :: logout unsuccessfull
		kill ${PIDS[*]} &> /dev/null
		killall "System Events"
		ERROR=255
		break
	else
		sleep .3
	fi
done

# cleanup temp files
rm -f /tmp/suspend.bash.sh
rm -f /tmp/suspend.applescript.aps
rm -f /tmp/suspend.success

# exit
#exit ${ERROR:- 0}
exit 0



