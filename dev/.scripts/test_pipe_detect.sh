#!/bin/bash

function pipe_dt(){
	echo -n

	exec 6<&0
	#exec 6< <(cat)

	#cat <<-ED | sed -f <(cat) <<< "test 123"
	cat <<-SED | sed -f <(cat) <&6 6<&-
		s/^/${FUNCNAME} /


	SED
	#cat | tee >(wc -c)
}
function pipe_dt2(){
	exec 6<&0
	exec <<< "howdy"
	while read line; do
		echo line = $line
	done
	cat <<-SED | sed -f <(cat) <&6 6<&-
		s/^/${FUNCNAME} /
	SED
}
function pipe_dt3(){
	exec 6<&0
	exec 2<<< "some error data"
	while read line; do
		echo line = $line
	done <&2
	cat <<-SED | sed -f <(cat) <&6 6<&-
		s/^/${FUNCNAME} /
	SED
}
function pipe_dt4(){
	exec < <(sed 1p 2>/dev/null)
	if read -t 0.01 line; then
		echo READ :: line = $line
		while read line; do
			echo line = $line
		done
	else
		echo READ :: NO-PIPE\!\!\!
	fi
	#cat <<-SED | sed -f <(cat) <&6 6<&-
	#	s/^/${FUNCNAME} /
	#SED
}
function dito_stream(){
	local descriptor=${1:-2}
	tee >(cat) 1>&${descriptor}
}
coproc p1 {
	exec 9>&1
	exec > /dev/null
	#exec 3> /dev/null
	echo `date` :: p1 :: START | tee "$0".log_p1
	while read line; do
		echo `date` :: p1 :: INPUT -- $line |
		tee -a "$0".log_p1
	done
# >/dev/null
	echo `date` :: p1 :: STOP | tee -a "$0".log_p1
	exec 1>&9 9>&-
}
function main(){
	echo -n
	echo ${FUNCNAME} :: START TEST; echo
	echo wow |	pipe_dt
	echo ${FUNCNAME} :: STOP TEST; echo

	echo \$0 :: `basename $0` | tee -a "$0".log_test
	echo alpha >&${p1[1]}
	echo beta >&${p1[1]}
	echo gama >&${p1[1]}

	#exec 7>&1
	#exec >&${p1[1]}
	exec > >(tee >(cat) >&${p1[1]})
	echo one111
	echo two
	echo three
	#exec 1>&7 7>&-


	echo ${FUNCNAME} :: STOP COPROC; echo
	
	echo DITO TEST2 | pipe_dt2
	echo DITO TEST3 | pipe_dt3
	echo DITO TEST4 | pipe_dt4
	pipe_dt4
	
	echo ${FUNCNAME} :: END DITO

	(
		echo out |
		dito_stream 2
		echo error 1>&2
	) 1>/dev/null

	echo ${FUNCNAME} :: LAST TEST
	echo zeta >&${p1[1]}
}
main "$@"
