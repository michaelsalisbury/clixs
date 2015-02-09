#!/bin/bash

source /etc/bash_completion.d/vboxmanage_completion.bash



COMP_WORDS=(vboxmanage "$@")
COMP_CWORD=${#COMP_WORDS[*]}
#_vboxmanage_raw_command_opts
_vboxmanage_commands |
while read cmd; do
	(( ${#cmd} )) || continue
	echo [${cmd}]
	_vboxmanage_actions2 ${cmd} | sed 's/^/    /'
	#_vboxmanage_command ${cmd}
done
#done <<-LIST
#	guestcontrol
#	list
#LIST
#_vboxmanage_first_arg setextradata



#_COMPREPLY_put_commands
#echo ${_COMPREPLY[*]}

#COMP_WORDS=(vboxmanage "$@")
#COMP_CWORD=${#COMP_WORDS[*]}
#_vboxmanage
#while read MATCH; do
#	printf "%q\n" "${MATCH}"
#done < <(printf "%q\n" "${COMPREPLY[@]}") | column
