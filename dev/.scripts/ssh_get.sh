#!/bin/bash

function ping_test(){
	local DELAY='25'
	local HOST=$1
	local PORT=${2:-22}
	local COUNT=${3:-5}
	nping --delay ${DELAY}ms -p ${PORT} -c ${COUNT} ${HOST} |
	awk  '/^TCP connection attempts/{print $8}'
}
function ssh_auth_test(){
	local HOST=$1
	ssh -q	-o ConnectTimeout=5\
		-o PasswordAuthentication=no\
		-o PubkeyAuthentication=yes\
		-o StrictHostKeyChecking=no\
		${HOST} 'hostname' &> /dev/null
	return $?
}
function ssh_one_liner(){
	local HOST=$1
	shift
	local CMD="$@"
	ssh -q	-o ConnectTimeout=5\
		-o PasswordAuthentication=no\
		-o PubkeyAuthentication=yes\
		-o StrictHostKeyChecking=no\
		${HOST} "${CMD}" 2>/dev/null
}
function main(){
	echo
	cat /etc/hosts |
	sed '/^[[:space:]]*$/d;/^[[:space:]]*#/d;/^[^[:digit:]]/d' |
	sort -u |
	while read IP HOSTNAME x y z; do
		{
			#echo `ping_test ${IP} 5` ${IP} ${HOSTNAME}
			local SSH=`ping_test ${IP} 22`
			local RDP=`ping_test ${IP} 3389`
			if (( SSH )); then
				ssh_auth_test ${IP}
				local AUTH=$?
			else
				local AUTH='999'
			fi
			if (( SSH )) && ! (( AUTH )); then
				local UBUNTU=$(ssh_one_liner ${IP} lsb_release -s -r)
				UBUNTU=${UBUNTU:-NA}
			else
				local UBUNTU='OTHER'
			fi


			#printf "%-3s%-3s%-5s%-16s%-30s%-20s%-20s%-20s\n" ${SSH} ${RDP} ${AUTH}  ${IP} ${HOSTNAME} $x $y $z
			if (( SSH )) && ! (( AUTH )); then
				printf "%-3s%-3s%-5s%-10s%-16s%-30s%-20s%-20s%-20s\n" ${SSH} ${RDP} ${AUTH} ${UBUNTU} ${IP} ${HOSTNAME} $x $y "$z"
			fi
			
		} &
	done
	echo
}
main "$@"
