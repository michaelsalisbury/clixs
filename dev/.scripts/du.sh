#!/bin/bash

function du_directory_sort(){
	#du -cksh "${1}"/{.[a-zA-Z]*,*} 2>(du_directory_sort_errors)
	#du -cksh "${1}"/{.[a-zA-Z]*,*} 2>&1
	#sed '/^du:/d'														|
	du -cksh "${1}"/{.[a-zA-Z]*,*} 2>(cat 2>&1 | du_directory_sort_errors)|
	sed 's/^0[[:space:]]/0.0\tB\t/'										|
	sed 's/^\([[:digit:]]\+\)\([BGKM]\)/\1.0\t\2/'						|
	sed 's/^\([[:digit:]\.]\+\)\([BGKM]\)/\1\t\2/'						|
	sort -k2,2 -k1n,1													|
	sed '/^[[:digit:]\.]\+[[:space:]]G[[:space:]]/{x;/^$/d;x;H;d};$G'
}
function du_directory_sort_errors(){
	#cat 2>&1 | grep -v cannot
	echo hi
	cat 2>&1  | grep -v access
	echo bye
}

if [ -d "${1}" ]; then
	du_directory_sort "${1}"
elif [[ "${1}" =~ ^-[cksh]$ ]] && [ -d "${2}" ]; then
	du_directory_sort "${2}"
else
	du "$@"
fi






#sed '/^[[:digit:]\.]\+[[:space:]]G[[:space:]]/{x;/^$/d;G;h;d};$G'
#sed '/^[[:digit:]\.]\+[[:space:]]G[[:space:]]/{H;d};$G'
#sed '/^$/d'
#sed '/^[[:digit:]\.]\+[[:space:]]G[[:space:]]/{H;d};${x;s/[[:space:]]*\n//;H;x}'


exit 0
du -cksh "${1}"/{.[a-zA-Z]*,*}					|
sed 's/^0[[:space:]]/B 0.\0/'					|
sed 's/^\([[:digit:]]\+\)\([BGKM]\)/\2 \1.0/'	|
sed 's/^\([[:digit:]\.]\+\)\([BGKM]\)/\2 \1/'






