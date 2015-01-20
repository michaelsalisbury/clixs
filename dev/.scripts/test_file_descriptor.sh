#!/bin/bash

BOOL=( false true )

# read only file descriptor

exec 3<<< "test read only file descriptor"
cat <&3 3<&-

exec 3< <(echo some command to test read only file descriptor)
echo `cat <&3 3<&-`


# read write file descriptor
exec 3<><(echo -n)
echo some data >&3
read -u 3 line
echo $line
DELAY='.1'
echo -n .; echo some more data `date` >&3; sleep ${DELAY}
echo -n .; echo some more data `date` >&3; sleep ${DELAY}
echo -n .; echo some more data `date` >&3; sleep ${DELAY}
echo    .; echo some more data `date` >&3; sleep ${DELAY}
# a read write file descriptor has no "end of file" byte so if it is read it will hang after the last line
# to easily read the current lines in the file descriptor add a line that can be found and force the loop to
# break on the last line read.
read BREAK < <(tee >(cat) >&3 <<< BREAK${RANDOM}BREAK )
while read -u 3 line; do
	[ "${line//[^BREAK[:digit:]]/}" == "${BREAK}" ] && break
	echo ${line}
done
exec 3<&-


# read write fd (file descriptor) where the descriptor is a variable
fd='3'
eval "exec ${fd}<><(echo -n)"
echo varable file descriptor >&3
read -u 3 line
echo $line
eval "exec ${fd}<&-"


# read write fd (file descriptor) where the descriptor is a variable and the data is fead back into the fd
fd='3'
eval "exec ${fd}<><(echo -n)"
DELAY='.1'
echo -n .; echo variable file descriptor refresh `date` > >(eval "cat >&${fd}"); sleep ${DELAY}
echo -n .; echo variable file descriptor refresh `date` > >(eval "cat >&${fd}"); sleep ${DELAY}
echo -n .; echo variable file descriptor refresh `date` > >(eval "cat >&${fd}"); sleep ${DELAY}
echo    .; echo variable file descriptor refresh `date` > >(eval "cat >&${fd}"); sleep ${DELAY}
read BREAK < <(tee >(cat) > >(eval "cat >&${fd}") <<< BREAK${RANDOM}BREAK )
while read -u ${fd} line; do
	[ "${line//[^BREAK[:digit:]]/}" == "${BREAK}" ] && break
	echo ${line} > >(eval "cat >&${fd}")
	echo ${line}
done

echo now lets read the one line of fd ${fd} for proof
read -u ${fd} line
echo ${line}
eval "exec ${fd}<&-"


# lets build some functions for utilizing fd's (file descriptors)
# my funcions focus on utilizing fd's without files, so basically as named pipes
# they do everything a named pipe does but will be automatically destroyed when the 
# script finishes
# -------------------------------------------------------------
# fdw    :: pipe date into a fd
# fdc    :: close a fd
# mkfdrw :: close a fd and re-open it in read/write mode
# fdrw   :: read all the lines in the fd, write the same lines back into the fd
# fdlc   :: return the line count of the fd
# fdrl   :: read x lines in the fd, read lines are not written back into the fd
#           an undefined line count will read all of the lines in the fd
#           a negatve line count will read from the bottom of the fd

function fdw(){
	local fd=${1:-3}
	eval "cat >&${fd}"
}
function fdc(){
	local fd=${1:-3}
	eval "exec ${fd}<&-"
}
function mkfdrw(){
	local fd=${1:-3}
	fdc ${fd}
	eval "exec ${fd}<><(echo -n)"
}
function fdrw(){
	local BREAK line fd=${1:-3}
	read BREAK < <(tee >(fdw ${fd}) <<< BREAK${RANDOM}BREAK )
	while read -u ${fd} line; do
		[ "${line//[^BREAK[:digit:]]/}" == "${BREAK}" ] && break
		tee >(fdw ${fd}) <<< "${line}"
	done
}
function fdlc(){
	local fd=${1:-3}
	fdrw ${fw} | wc -l
}
function fdrl(){
	# if arg #2 (line count) is unset then all lines in the fd are emptied to stout
	# arg #2 will read from the top (positive integer) or bottom (negative integer) of the fd
	# this function needs a global array: BOOL=( false true )
	local BREAK line fd=${1:-3} lines=${2:-1} read_stack_from_top=${BOOL[(( ${2:-1} >= 0 ))]}
	# to read lines at the bottom of the stack we need to know how many lines are in the stack
	${read_stack_from_top} || local fdl_count=$(fdlc ${fd})
	# we need to add a unique line to the bottom of the stack in order to know when to break the while loop
	read BREAK < <(tee >(fdw ${fd}) <<< BREAK${RANDOM}BREAK )
	while read -u ${fd} line; do
		[ "${line//[^BREAK[:digit:]]/}" == "${BREAK}" ] && break
		if ${read_stack_from_top}; then
			(( lines${2:+--} > 0 )) && cat <<< "${line}" || fdw <<< "${line}"
		else
			(( lines + fdl_count-- > 0 )) && fdw <<< "${line}" || cat <<< "${line}"
		fi
	done
}

# now lets test those function
fd='3'
mkfdrw ${fd}
DELAY='.1'
echo -n .; echo 1 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 2 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 3 function test `date` | fdw ${fd}; sleep ${DELAY}
echo    .; echo 4 function test `date` | fdw ${fd}; sleep ${DELAY}
echo FUNCTION TEST :: lets rw the file descriptor \(this loops the data back into the fd\)
fdrw ${fd}
echo FUNCTION TEST :: lets r the file descriptor \(this reads the data emptying the fd\)
fdrl ${fd}
echo FUNCTION TEST :: lets r the file descriptor again, it should be empty
fdrl ${fd}
echo FUNCTION TEST :: lets write some new data into the fd
echo -n .; echo 1 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 2 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 3 function test `date` | fdw ${fd}; sleep ${DELAY}
echo    .; echo 4 function test `date` | fdw ${fd}; sleep ${DELAY}
echo FUNCTION TEST :: lets r 2 lines
fdrl ${fd} 2
echo FUNCTION TEST :: lets rw the remaining lines there should be 2
fdrw ${fd}
echo FUNCTION TEST :: lets r 10 lines, there are only 2 but this should work
fdrl ${fd} 10
echo FUNCTION TEST :: lets write some new data into the fd
echo -n .; echo 1 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 2 function test `date` | fdw ${fd}; sleep ${DELAY}
echo -n .; echo 3 function test `date` | fdw ${fd}; sleep ${DELAY}
echo    .; echo 4 function test `date` | fdw ${fd}; sleep ${DELAY}
echo FUNCTION TEST :: lets see if we can pull the last 2 lines
fdrl ${fd} -2
echo FUNCTION TEST :: lets read whatever is left
fdr ${fd}
echo FUNCTION TEST :: lets close the file descriptor
fdc ${fd}


