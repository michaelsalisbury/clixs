#!/bin/bash
function nmap_bounce_ssh_rdp(){
	local NET=$1
	nmap -sn -PA 22,3389 ${NET} &>/dev/null
}
function nmap_bounce_net(){
	local NET=$1
	local PORTS=${2:-22,3389}
	nmap -sn -PA${PORTS} ${NET} 2>/dev/null |
	grep -q "Host is up"
}
function nmap_is_host_up(){
	local IP=$1
	local PORTS=${2:-22,3389}
	nmap --host-timeout 1 -sn -PA${PORTS} ${IP} 2>/dev/null |
	grep -q "Host is up"
}
function mac_add_delim(){
	local MAC=$1
	local DELIM=${2:-:}
	MAC=${MAC//[^[:alpha:][:digit:]]/}
	MAC=${MAC:0:2}${DELIM}${MAC:2:2}${DELIM}${MAC:4:2}${DELIM}${MAC:6:2}${DELIM}${MAC:8:2}${DELIM}${MAC:10:2}
	echo ${MAC}
}
function arp_mac_to_ip(){
	local INT=$1
	local MAC=$(mac_add_delim $2 :)
	cat <<-AWK | awk -f <(cat) <(/usr/sbin/arp -an -i ${INT})
		\$4=="${MAC,,}"{
			gsub("[()]","",\$2)
			print \$2
		}
	AWK
}
function vbox_chmod_vm_vcp_files(){
	local NAME=$1
	local CHMOD=${2:-+r}
	local VAR='vcpfile'
	if vbox_does_machine_exist "${NAME}"; then
		vbox_get_vminfo "${NAME}" ${VAR} &>/dev/null
		eval ${VAR}=\${!VAR%\.webm}
		echo ${!VAR}
		chmod -v ${CHMOD} "${!VAR}"*".webm" |
		grep changed
	fi
}
function vbox_get_vm_vcp_file(){
	local NAME=$1
	local VAR='vcpfile'
	if vbox_does_machine_exist "${NAME}"; then
		vbox_get_vminfo "${NAME}" ${VAR}
	fi
}
function vbox_get_vm_cfg_file(){
	local NAME=$1
	local VAR='CfgFile'
	if vbox_does_machine_exist "${NAME}"; then
		vbox_get_vminfo "${NAME}" ${VAR}
	fi
}
function vbox_get_vm_log_file(){
	local NAME=$1
	local VAR='LogFldr'
	if vbox_does_machine_exist "${NAME}"; then
		vbox_get_vminfo "${NAME}" ${VAR} &>/dev/null
		local LOG=$(find "${!VAR}" -name \*.log)
		echo ${LOG}
	fi
}
function vbox_is_machine_running(){
	local NAME=${*:-NULL}
	vbox_is_machine_state "${NAME}" running
}
function vbox_is_machine_paused(){
	local NAME=$1
	vbox_is_machine_state "${NAME}" paused
}
function vbox_is_machine_poweroff(){
	local NAME=$1
	vbox_is_machine_state "${NAME}" poweroff
}
function vbox_is_machine_state(){
	local NAME=$1
	local STATE=${2:-running}
	vbox_get_vminfo "${NAME}" VMState |
	grep -q ^${STATE}\$
}
function vbox_get_vm_state(){
	local NAME=$1
	local VAR='VMState'
	vbox_get_vminfo "${NAME}" ${VAR}
}
function vbox_get_vm_adapter_mac(){
	local NAME=$1
	local VAR=${2/nic/macaddress}	# nic[1-8]
	local MAC
	vbox_get_vminfo "${NAME}" ${VAR}
}
function vbox_get_vm_adapter_bridged_int(){
	local NAME=$1
	local VAR=${2/nic/bridgeadapter} # nic[1-8]
	vbox_get_vminfo "${NAME}" ${VAR}
}
function vbox_get_vm_adapters_bridged(){
	local NAME=$1
	if vbox_does_machine_exist "${NAME}"; then
		cat <<-SED | sed -n -f <(cat) <(vboxmanage showvminfo "${NAME}" -machinereadable)
			/^nic[1-8]=/{
				/="bridged"$/!d
				s/=.*//p
			}
		SED
	fi
}
function vbox_get_vm_adapters(){
	local NAME=$1
	if vbox_does_machine_exist "${NAME}"; then
		cat <<-SED | sed -n -f <(cat) <(vboxmanage showvminfo "${NAME}" -machinereadable)
			/^nic[1-8]=/{
				/="none"$/d
				s/=.*//p
			}
		SED
	fi
}
function vbox_get_vminfo(){
	local NAME=$1
	if [ "${VAR}" != "$2" ]; then
		local VAR=$2
		eval local $2
	fi
	if vbox_does_machine_exist "${NAME}"; then
		eval $(
			vboxmanage showvminfo "${NAME}" -machinereadable |
			grep ^${VAR}=
		)
		echo "${!VAR}"
	fi
}
function load(){
	local VAR=$1
	shift
	read -d $'' $VAR < <("$@")
}
function now(){
	local PRECISION=${1:-4}
	local NOW=$(date "+%s%N")
	echo ${NOW:0: -9 + ${PRECISION}}
}
function vbox_does_machine_exist(){
	local NAME=${*:-NULL}
	vboxmanage list vms |
	grep -q "${NAME}"
}
function vbox_list_running_names(){
	vboxmanage list runningvms |
	sed 's/\(^"\|"[[:space:]].*$\)//g'
}
function vbox_list_running_uuid(){
	vboxmanage list runningvms |
	awk '{print $NF}'
}
function switch_set_default(){
	local ARG=\-${1//-/}    # switch/arg
	local VALUE=$2          # default value for switch/arg
	# echo command for eval with calling function
	cat <<-EVAL
		eval [[ "\$*" =~ ${ARG}[[:space:]]- ]]  \
		|| [ "${ARG}" == "\${@: -1}" ]          \
		&& set -- "\${@/${ARG}/${ARG}${VALUE}}"
	EVAL
}
