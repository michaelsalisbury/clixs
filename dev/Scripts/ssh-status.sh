#!/bin/bash

# true or false on system online status
function system_online(){
	local TARGET=$1
	ssh					\
		-o ConnectionAttempts=1		\
		-o ConnectTimeout=1		\
		-o PasswordAuthentication=no	\
		${TARGET} date &>/dev/null
}
function system_ping(){
	local TARGET=${1#*@}
	if ping -w 1 -c 1 ${TARGET} &>/dev/null; then
		sleep .7
		return 0
	else
		return 1
	fi
}
# true or false on intervale lapse
function interval_has_lapsed(){
	local INTERVAL=$1
	local GLOBAL_TIMER_VARIABLE=$2
	local TIME=$(date '+%s')
	local LAPSE=$(( TIME - ${!GLOBAL_TIMER_VARIABLE} ))
	if (( LAPSE >= INTERVAL )); then
		return 0
	else
		return 1
	fi
}
# reset timer variable, closing one or more interval periods
function close_timer_intervals(){
	local INTERVAL=$1
	local GLOBAL_TIMER_VARIABLE=$2
	local TIME=$(date '+%s')
	local COUNT=$(( ( TIME - ${!GLOBAL_TIMER_VARIABLE} ) / INTERVAL ))
	eval ${GLOBAL_TIMER_VARIABLE}=$(( INTERVAL * COUNT + ${!GLOBAL_TIMER_VARIABLE} ))
}
# echo something if the interval period has lapsed
function echo_on_interval(){
	local INTERVAL=$1
	local GLOBAL_TIMER_VARIABLE=$2
	shift 2
	local CHAR=$*
	if interval_has_lapsed ${INTERVAL} ${GLOBAL_TIMER_VARIABLE}; then
		close_timer_intervals ${INTERVAL} ${GLOBAL_TIMER_VARIABLE}
		echo -n ${CHAR}
	fi
}

interval_in_seconds='3'
interval_timer=$(date '+%s')

while true; do
	# system is offline; no ping
	interval_timer=$(date '+%s')
	echo -n System Offline :: $1......
	tput sc
	time while ! system_ping $1; do tput rc; echo -n $(( $(date '+%s') - interval_timer)); done

	# system is pinging but ssh is offline
	interval_timer=$(date '+%s')
	echo -n System pinging but ssh unavailable :: $1......
	tput sc
	time while ! system_online $1; do tput rc; echo -n $(( $(date '+%s') - interval_timer)); done
	
	# system is online and ssh is available
	interval_timer=$(date '+%s')
	echo -n System Online :: $1......
	tput sc
	time while   system_online $1; do tput rc; echo -n $(( $(date '+%s') - interval_timer)); done

	# system is offline; no ping
	interval_timer=$(date '+%s')
	echo -n System pinging :: $1......
	tput sc
	time while   system_ping $1; do tput rc; echo -n $(( $(date '+%s') - interval_timer)); done
done

exit

while true; do
	echo -n System Offline :: $1.
	time while ! system_online $1; do echo_on_interval ${interval_in_seconds} interval_timer '.'; done
	echo -n System Online :: $1.
	time while   system_online $1; do echo_on_interval ${interval_in_seconds} interval_timer '.'; done
done

exit

echo -n System Offline :: $1.
time while ! ssh				\
		-o ConnectionAttempts=1		\
		-o ConnectTimeout=1		\
		-o PasswordAuthentication=no	\
		"$@" &>/dev/null
	do echo -n .
done
echo -n System Online :: $1.


