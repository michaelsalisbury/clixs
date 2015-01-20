#!/bin/bash

function main(){
	#echo STDOUT :: `date`
	#echo STDERR :: `date` 1>&2
	#local filter; read filter <<< "wow"
	#echo ${filter}

	echo one two three | replay


}
function replay(){
	#tee >(cat) >&0
	cat
	#DATA=1234 echo ${DATA[@]}



}

main "$@"
#main "$@" 3> >(cat >> ~/.logs/test.stdout) 4> >(cat 
