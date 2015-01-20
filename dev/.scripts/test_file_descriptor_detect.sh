#!/bin/bash

function main(){
	echo -n
	has_pipe <<< "PIPE"
	has_pipe


}
function has_pipe(){
	echo TEST00 ::
	exec 3<<< "test"	
	cat <&3 3<&-
	#echo
	#echo TEST01 ::
	#exec 4< <(cat)
	#cat <&4 4<&-
	echo
	echo TEST02 ::
	#exec 5<><(echo 1234; cat)
	exec 5<><(echo -n)
	echo START >&5
	#exec 5<&0								# does not work
	
	echo END >&5
	while read -u 5 line; do
		echo TEST02 :: ${line}
		[ "${line:0:3}" == "END" ] && break
	done

	echo
	echo TEST03 ::
	exec 7< <(sed 1p 2>/dev/null)
	
	#exec 6<><(while read -u 7 -t 1 line; do echo "${line}"; done; echo END)

	if read -t 0.2 -u 7 line; then
		echo has PIPE
		cat <&7
	else
		echo no PIPE
	fi

	exec 7<&-
	exec 5<&-
	exec 3<&-
	#cat <&6 6<&-


		
		

}
main "$@"

