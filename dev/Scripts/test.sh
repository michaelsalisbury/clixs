#!/bin/bash


echo -n 1234567890
tput sc
echo -n Hello
tput rc
echo tadaaaaaa


exit

tmp=/dev/shm

function GET_PPID(){
	ps --no-heading -o ppid -p $1
}
function GET_PID(){
	local depth=${1:-1}			# default depth to get PPID
	local fifo=${tmp}/${FUNCNAME}.fifo	# fifo; to create background process
	mkfifo     "${fifo}"			# create fifo
	cat        "${fifo}" &>/dev/null &	# create background process
	local   PID=$(GET_PPID $!)		# get parent PID to background process
	while (( depth-- )); do			# get parent to the parent till depth reached
		PID=$(GET_PPID ${PID})
	done
	echo -n >> "${fifo}"			# close background process
	rm -f      "${fifo}"			# clean-up fifo
	echo ${PID}				# return results
}

function main(){
	echo $$ :: $PPID
	echo ----- -- ----- -- ----- -- -----
	test
	echo
	echo  `test 2`
	echo
	echo $(test 2)

}
main "$@" | column -t
