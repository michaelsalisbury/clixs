#!/bin/bash


function main(){
	FIFO_OUT=/dev/shm/$$FIFO_OUT
	FIFO_IN=/dev/shm/$$FIFO_IN

	mkfifo "${FIFO_OUT}"
	mkfifo "${FIFO_IN}"

	feed

	
	echo hello
	for i in {1..100}; do
		echo $i > "${FIFO_IN}"
		sleep .2
		echo "# $i" > "${FIFO_IN}"
		sleep .2
	done

	sleep 3
	echo DONE

}
function feed(){
	while true; do
		while read line; do
			echo ${line}
		done < "${FIFO_IN}"
	done | prog &
	feed_PID=$!
}
function prog(){
	zenity --progress	\
	--title="1234"		\
	--text="start"		\
	--auto-close		\
	--pulsate		\
	--percentage=0

	echo $?
	echo feed_PID :: ${feed_PID}

}





main
rm -rf /dev/shm/$$*
