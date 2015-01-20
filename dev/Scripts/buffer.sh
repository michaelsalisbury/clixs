#!/bin/bash

parsers=3
fifodir=/tmp/buffer
tmp=/dev/shm/$$


monitor=/tmp/buffer/monitor
monitorPID_FQFN=/tmp/buffer/monitor.pid
output_=/tmp/buffer/output_
PIDFILE=/tmp/buffer/PID
#killtree='s/[^(]\+\(([0-9]\+)\)/\1\n/g;s/[()]//g'
killtree='s/[^(]\+\(([0-9]\+)\)/\1/g;s/[()]/ /g'

########################################### Auto Generate Handlers
function GEN_HANDLERS(){
	local handler="/tmp/$$$(basename "${BASH_SOURCE}")_handler$$"
	touch "${handler}"
	local cnt
	for cnt in `seq ${parsers}`; do
		cat <<-HANDLER > "${handler}"
			function parser${cnt}_handler(){
				local parser=\$(GET_FIFO_FQFN parser \${FUNCNAME//[^0-9]/})
				local output=\$(GET_FIFO_FQFN output \${FUNCNAME//[^0-9]/})
				cat "\${parser}" | sed "s/^/\$(date) :: /" >> "\${output}"
			}
		HANDLER
		source "${handler}"
	done
	rm "${handler}"
}
GEN_HANDLERS
##################################################################
function main(){
	KILL_PID_FILE "${PIDFILE}"
	daemon &
	wait
}
function GET_PPID(){
	ps --no-heading -o ppid -p $1
}
function GET_PID(){
	local pid_depth=${1:-1}			# default depth to get PPID
	local fifo=${tmp}/${FUNCNAME}.fifo	# fifo; to create background process
	mkdir -p  "${tmp}"
	mkfifo     "${fifo}"			# create fifo
	cat        "${fifo}" &>/dev/null &	# create background process
	local   PID=$(GET_PPID $!)		# get parent PID to background process
	while (( pid_depth-- )); do		# get parent to the parent till depth reached
		PID=$(GET_PPID ${PID})
	done
	echo -n >> "${fifo}"			# close background process
	rm -f      "${fifo}"			# clean-up fifo
	echo ${PID}				# return results
}
function GET_FIFO_FQFN(){
	local places=${#parsers}
	local name=${1}
	local cnt=${parsers//?/0}${2}
	      cnt=${cnt: -${places}}
	echo "${fifodir}/${name}${cnt}"
}
function KILL_PID_FILE(){
	# test file exists
	[ -f "$1" ] || return 1
	# test pid in file is alive
	cat "$1" | xargs ps --no-heading -p | grep -q "" || return 1
	# kill pid
	cat "$1" | xargs kill
}
function WRITE_PIDFILE(){
	local file=$1
	local dir=$(dirname "${file}")
	local pid_depth=${2:-1}
	mkdir -p "${dir}"
	GET_PID ${pid_depth} | tee "${file}"
}
function daemon_exit(){
	local PID=$1
	pstree -p ${PID} | tr -s -c [:digit:] ' ' | xargs kill
	sleep 1
	rm -rf "${tmp}"
	rm -rf "${fifodir}"
}
function daemon(){
	local PID=$(WRITE_PIDFILE "${PIDFILE}" 2)	
	trap "daemon_exit ${PID}" TERM
	launch_monitor &
	wait
}
function buffer(){
	local parser=${1:-${parsers}}
	local fifo=$(GET_FIFO_FQFN buffer ${parser})
	let parser--
	if (( ! ${#1} )); then
		cat "${monitor}" |\
		tee -a "${fifo}" |\
		buffer ${parser} &
		jobs -p > "${monitorPID_FQFN}"
		jobs -p | sed 's/^/jobs -p :: /' >> "${output_}"
		jobs -l | sed 's/^/jobs -l :: /' >> "${output_}"
		wait
	elif (( parser )); then
		tee -a "${fifo}" | buffer ${parser}
	else
		tee -a "${fifo}" &>/dev/null
	fi
}
function launch_monitor(){
	# local variables
	local cnt buffer parser
	# setup tmp
	mkdir -p "${fifodir}"
	# setup input fifo
	mkfifo "${monitor}" &>/dev/null
	# setup buffer and parser fifo's
	for cnt in `seq ${parsers}`; do
		mkfifo `GET_FIFO_FQFN buffer ${cnt}` &>/dev/null
		mkfifo `GET_FIFO_FQFN parser ${cnt}` &>/dev/null
	done
	# setup stream splitter
	while :; do
		buffer
	done &

	# setup buffer and parser loops
	for cnt in `seq ${parsers}`; do
		buffer=$(GET_FIFO_FQFN buffer ${cnt})
		parser=$(GET_FIFO_FQFN parser ${cnt})
		while :; do
			dd if="${buffer}" of="${parser}" conv=notrunc &>/dev/null
		done &
		while :; do
			parser${cnt}_handler
		done &
	done
	wait
}
function monitor_bump(){
	echo -n >> "${monitor}"
}
function monitor_kill(){
	local PID=$(cat "${monitorPID_FQFN}")
	if ps --no-heading -p ${PID} &>/dev/null; then
		kill ${PID}
	else
		monitor_kill
	fi
}
##################################################################
function parser1_handler(){
	local parser=$(GET_FIFO_FQFN parser ${FUNCNAME//[^0-9]/})
	local output=$(GET_FIFO_FQFN output ${FUNCNAME//[^0-9]/})
	while read line; do
		echo "${line}" | sed "s/^/`date` :: /" >> "${output}"
		sleep .5
	done < <(cat "${parser}")
	monitor_kill
}
function parser2_handler(){
	local parser=$(GET_FIFO_FQFN parser ${FUNCNAME//[^0-9]/})
	local output=$(GET_FIFO_FQFN output ${FUNCNAME//[^0-9]/})
	cat "${parser}" |\
	sed -n '/^bash/s/^bash//p' |\
	tee -a "${output}".sh |\
	bash 1>> "${output}" 2>> "${output}".err
}
function parser2_handler_OLD(){
	local parser=$(GET_FIFO_FQFN parser ${FUNCNAME//[^0-9]/})
	local output=$(GET_FIFO_FQFN output ${FUNCNAME//[^0-9]/})
	local -a words
	while read line; do
		eval words=( "${line}" )
		echo words[0] :: ${words[0]}   >> "${output}"
		echo words[1] :: ${words[*]:1} >> "${output}"
		if [ -f "${words[0]}" ] || [ -p "${words[0]}" ]; then
			eval "${words[*]:1}" 1>> "${words[0]}" 2>> "${words[0]}"
		fi
	done < <(cat "${parser}")
}


main "$@"
