#!/bin/bash

# AWK function
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
	local DISK_LIST
	read -d $'' DISK_LIST <<-DISK_LIST
		/dev/disk/by-id/ata-WDC_WD30EFRX-68EUZN0_WD-WMC4N1318393
		/dev/disk/by-id/ata-ST3640623AS_9VK0FNH1
	DISK_LIST
	local DISK_LIST=$(ls -1 /dev/disk/by-id/* | sed -n '/part/d;/ata/p')

	local _DEV
	while read _DEV; do
		DRIVE_ID=$(basename $_DEV)
		DRIVE_LETTER=$(basename $(readlink -f "${_DEV}"))
		SMART_REPORT="/dev/shm/$$_${FUNCNAME}_${DRIVE_LETTER}_${DRIVE_ID}"
		[ "${DRIVE_LETTER:0:2}" == "sd" ] || continue # device must be hd
		smartctl -a ${_DEV} > "${SMART_REPORT}" &
	done <<< "${DISK_LIST}"
	wait
	
	while read _DEV; do
		DRIVE_ID=$(basename $_DEV)
		DRIVE_LETTER=$(basename $(readlink -f "${_DEV}"))
		SMART_REPORT="/dev/shm/$$_${FUNCNAME}_${DRIVE_LETTER}_${DRIVE_ID}"
		[ "${DRIVE_LETTER:0:2}" == "sd" ] || continue # device must be hd
		echo -n "${DRIVE_LETTER}"\ 
		#get_smartctl_info Model Family               < "${SMART_REPORT}" | tr \  _
		#echo -n \  
		get_smartctl_info_capacity                   < "${SMART_REPORT}"
		echo -n \  
		get_disk_by_id_path                          < "${SMART_REPORT}"
		echo -n \ 
		get_smartctl_attrib 10 Temperature_Celsius   < "${SMART_REPORT}"
		echo -n \ 
		get_smartctl_attrib 10 Reallocated_Sector_Ct < "${SMART_REPORT}"
		echo -n \ 
		get_smartctl_attrib 10 Power_Cycle_Count     < "${SMART_REPORT}"
		echo -n \ 
		get_power_on_hours                           < "${SMART_REPORT}"
		echo -n \ 
		echo -n `get_alignment ${_DEV}`
		echo -n /
		get_smartctl_info_sector_size                < "${SMART_REPORT}"
		echo -n \ 
		get_partition_type ${_DEV}
		echo
	done <<< "${DISK_LIST}" |
	sort |
	sed '1i DEV SIZE __ ://dev/disk/by-id/ata-[MODEL]_[SERIAL] C RSC PCC HOURS ALGN/SECS TABLE . ' |
	column -t -s ' '
	#sed '1i DEV FAMILY SIZE __ ://dev/disk/by-id/ata-[MODEL]_[SERIAL] C RSC PCC HOURS ALGN/SS TABLE' |
	#sed '1i DEV FAMILY SIZE __ PATH:/dev/disk/by-id/ata-[MODEL]_[SERIAL] C SEC HOURS ALIGN TABLE' |

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
			if (\$2 == "${_key}") {F=1; printf \$${_field} }
		}
		END{
			if(!F)printf "-"
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
			echo -n ${_path} | sed 's/ata-//'
		else
			echo -n NA
		fi
	fi
}
function get_smartctl_info_sector_size(){
	exec 7< <(get_smartctl_info Sector Size[s]?)
	cat <<-AWK | awk -f <(cat) <&7 7<&-
		/logical/&&/physical/{
			match(\$0,/[0-9]*[^0-9]+$/)
			\$0=substr(\$0,RSTART,RLENGTH)
			printf \$1
		}
	AWK
}
function get_smartctl_info_capacity(){
	#exec 6<&0
	#exec 7< <(get_smartctl_info User Capacity <&6 6<&-)
	exec 7< <(get_smartctl_info User Capacity)
	cat <<-AWK | awk -f <(cat) <&7 7<&-
		${regsplit}
		BEGIN { REG = "\[[\.0-9 GMTB]*\]" }
		{
			#STR = \$0
			#STR = regsplit(STR,KEY_VAL,REG)
			STR = regsplit(\$0,KEY_VAL,REG)
			gsub(/[\[\]]/,"",STR)
			printf STR
			#printf regsplit(STR,KEY_VAL,REG)
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
function get_alignment(){
	(( ${#@} )) || EXIT_ERROR 1
	local _DEV=$1
	sgdisk -D "${_DEV}" |
		tail -1 |
		xargs echo -n
}
function get_partition_type(){
	(( ${#@} )) || EXIT_ERROR 1
	local _DEV=$1
	gdisk -l "${_DEV}" |
		grep -A 4 "Partition table scan:" |
		tail -n +2 |
		sed 's/ //g;/notpresent/d' |
		cut -f1 -d: |
		xargs echo -n |
		tr \  ,
}
function EXIT_ERROR(){
	
	exit $1
}
main "$@"
