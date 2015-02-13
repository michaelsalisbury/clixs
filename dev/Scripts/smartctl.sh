#!/bin/bash

read -d $'' regsplit <<-AWK
		function regsplit(str,array,regexp)
		{	dif = match(str,regexp) ? substr(str,RSTART,RLENGTH) : ""
			split(str, array, dif )
			return dif }
AWK

read -d $'' drives <<-DRIVES
	/dev/sda
	/dev/sdb
	/dev/sdc
	/dev/sdd
	/dev/sde
	/dev/sdf
	/dev/sdg
	/dev/sdh
	/dev/sdi
	/dev/sdj
DRIVES

read -d $'' trash <<-TRASH
Model Family:     Hitachi Deskstar 7K2000
Device Model:     Hitachi HDS722020ALA330
Serial Number:    JK1130YAHDEZMT
LU WWN Device Id: 5 000cca 221d3c077
Firmware Version: JKAOA28A
User Capacity:    2,000,398,934,016 bytes [2.00 TB]
Sector Size:      512 bytes logical/physical
Rotation Rate:    7200 rpm
Device is:        In smartctl database [for details use: -P show]
ATA Version is:   ATA8-ACS T13/1699-D revision 4
SATA Version is:  SATA 2.6, 3.0 Gb/s
Local Time is:    Sun May 18 22:04:04 2014 UTC
SMART support is: Available - device has SMART capability.
SMART support is: Enabled
TRASH


function main(){
	local _DEV
	while read _DEV; do
		smartctl -a ${_DEV} > /dev/shm/$$_${FUNCNAME}${_DEV//\//_} &
	done <<< "${drives}"
	wait
	
	
	while read _DEV; do
		echo -n $(basename ${_DEV})\ 
		get_smartctl_info Model Family               < /dev/shm/$$_${FUNCNAME}${_DEV//\//_} | tr \  _
		echo -n \  
		get_smartctl_info_capacity                   < /dev/shm/$$_${FUNCNAME}${_DEV//\//_}
		echo -n \  
		get_disk_by_id_path                          <  /dev/shm/$$_${FUNCNAME}${_DEV//\//_}
		echo -n \ 
		get_smartctl_attrib 10 Reallocated_Sector_Ct < /dev/shm/$$_${FUNCNAME}${_DEV//\//_}
		echo -n \ 
		get_power_on_hours                           < /dev/shm/$$_${FUNCNAME}${_DEV//\//_}
		echo -n \ 
		echo -n $(sgdisk -D ${_DEV})


		echo
	done <<< "${drives}" |
	sed '1i DEV FAMILY SIZE ___ PATH:/dev/disk/by-id/ata-[MODEL]_[SERIAL] SEC HOURS ALIGN' |
	column -t

	#cat   /dev/shm/$$_${FUNCNAME}_dev_sdj
	#echo ------------------------------------------------
	#get_power_on_hours < /dev/shm/$$_${FUNCNAME}_dev_sdj


	rm -f /dev/shm/$$_${FUNCNAME}_dev_*
}
function get_smartctl_attrib(){
	(( ${#@} )) || EXIT_ERROR 1
	local _field=$1
	shift
	local _key=$*
	exec 7<<-AWK
		${regsplit}
		{
			if (\$2 == "${_key}") { printf \$${_field} }
		}
	AWK
	awk -f <(cat <&7 7<&-)


	return
	exec 6<&0
	cat <<-AWK | awk -f <(cat) <&6 6<&-
		${regsplit}
		{
			if (\$2 == "${_key}") { printf \$${_field} }
		}
	AWK
}
function get_power_on_hours(){
	if [ "${FUNCNAME}" == "${FUNCNAME[1]}" ]; then
		get_smartctl_attrib 10 Power_On_Hours
	else
		local _hours=$(${FUNCNAME})
		local _years=$(( _hours / 24 / 7 / 52 ))
		local _hours=$(( _hours % ( 24 * 7 * 52 ) ))
		local _weeks=$(( _hours / 24 / 7 ))
		local _hours=$(( _hours % ( 24 * 7 ) ))
		local  _days=$(( _hours / 24 ))
		local _hours=$(( _hours % 24 ))
		echo -n ${_years}y,${_weeks}w,${_days}d,${_hours}h
	fi
}
function get_disk_by_id_path(){
	if [ "${FUNCNAME}" == "${FUNCNAME[1]}" ]; then
		exec 6<&0
		cat <<-AWK | awk -f <(cat) <&6 6<&-
			${regsplit}
			BEGIN {
				REG = ": +"
				KEY_SER = "Serial Number"
				KEY_MOD = "Device Model"
			}
			{
				STR = \$0
				DIF = regsplit(STR,KEY_VAL,REG)
				if (KEY_VAL[1] == KEY_SER) VAL_SER = KEY_VAL[2]
				if (KEY_VAL[1] == KEY_MOD) VAL_MOD = KEY_VAL[2]
			}
			END {
				sub(" ","_",VAL_MOD)
				printf                  "ata-"VAL_MOD"_"VAL_SER
				#printf "/dev/disk/by-id/ata-"VAL_MOD"_"VAL_SER
			}
		AWK
	else
		local _path=$(${FUNCNAME})
		if [ -L "${_path}" ]; then
			echo -n ${_path}
		elif [ -L "/dev/disk/by-id/${_path}" ]; then
			echo -n ${_path}
		else
			echo -n NA
		fi
	fi
}
function get_smartctl_info_capacity(){
	#exec 6<&0
	#exec 7< <(get_smartctl_info User Capacity <&6 6<&-)
	exec 7< <(get_smartctl_info User Capacity)
	cat <<-AWK | awk -f <(cat) <&7 7<&-
		${regsplit}
		BEGIN { REG = "\[[\.0-9 GMTB]*\]" }
		{
			STR = \$0
			printf regsplit(STR,KEY_VAL,REG)
		}
	AWK
}
function get_smartctl_info(){
	(( ${#@} )) || EXIT_ERROR 1
	local _key=$*
	exec 6<&0
	cat <<-AWK | awk -f <(cat) <&6 6<&-
		${regsplit}
		BEGIN { REG = ": +" }
		{
			STR = \$0
			DIF = regsplit(STR,KEY_VAL,REG)
			if (KEY_VAL[1] ~/^${_key}\$/) { printf KEY_VAL[2] }
		}
	AWK
}
function EXIT_ERROR(){
	
	exit $1
}
main "$@"