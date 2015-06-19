#!/bin/bash

function main(){
	#echo STDOUT :: `date`
	#echo STDERR :: `date` 1>&2
	#local filter; read filter <<< "wow"
	#echo ${filter}

	#echo one two three | replay

	exec 3<> <(cat &)
	echo hello >&3
	read data < <(cat <&3)
	echo wow :: $data
	echo goomba >&3
	#echo wonba  >>&3
	read data < <(cat <&3)
	echo wow :: $data
	

	echo 

	exec 3<&-

}
function replay(){
	echo -n
	#tee >(cat) >&0
	#cat
	#DATA=1234 echo ${DATA[@]}



}

main "$@"
#main "$@" 3> >(cat >> ~/.logs/test.stdout) 4> >(cat 
