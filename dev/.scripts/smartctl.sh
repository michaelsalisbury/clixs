#!/bin/bash
bob=0001230045
echo "${bob##'+(0)'}"



exit

function main(){

	local DISK ID DETAILS SIZE OVERALL
	while read DISK ID; do
		DETAILS=$(get_disk_details ${DISK})
		   SIZE=$(get_disk_size    ${DISK})
		   TIME=$(get_power_on_time         ${ID})
		 HEALTH=$(get_overall_health        ${ID})
		  COUNT=$(get_reallocated_sector_ct ${ID})
		echo $DISK $ID ${TIME} ${COUNT} ${HEALTH} ${DETAILS}
	done < <(list_disks) |\
	column -t |\
	tee /tmp/disks

}
function list_disks(){
	ls -l /dev/disk/by-id/ |\
	grep -v part |\
	grep -v \/sr[0-9] |\
	grep ata |\
	awk '{gsub(/[^a-z]/,"",$NF); print $NF" "$9}' |\
	sort
}
function get_disk_size(){
	local DISK=$1
	get_disk_details ${DISK} | awk '{print $1}'
}
function get_disk_details(){
	local DISK=$1
	echo "${LSHW}" | sed -n "s/.*${DISK} *disk *//p"
}
function get_smart_report_object(){
	local ID=$1
	local FIELD=$2
	shift 2
	local QUERY="$*"
	smartctl -a /dev/disk/by-id/${ID}	|\
	grep "${QUERY}"						|\
	awk {print\$${FIELD}}
}
function get_overall_health(){
	local ID=$1
	local FIELD='NF'
	local QUERY='overall-health'
	# get total hours
	local HEALTH=$(get_smart_report_object ${ID} ${FIELD} ${QUERY})
	echo ${HEALTH}
}
function get_reallocated_sector_ct(){
	local ID=$1
	local FIELD='NF'
	local QUERY='Reallocated_Sector_Ct'
	# get total hours
	local COUNT=$(get_smart_report_object ${ID} ${FIELD} ${QUERY})
	local FIELD='6'
	local THRESH=$(get_smart_report_object ${ID} ${FIELD} ${QUERY})
	echo ${COUNT}/"${THRESH##+(0)}"
}
Reallocated_Sector_Ct
function get_power_on_time(){
	local DELIM='.'
	local ID=$1
	local FIELD='NF'
	local QUERY='Power_On_Hours'
	# get total hours
	local HOURS=$(get_smart_report_object ${ID} ${FIELD} ${QUERY})
	# break down total hours
	local YEARS=$(( HOURS / 24 / 365 ))
	local DAYS=${DELIM}${DELIM}$(( HOURS / 24 % 365 ))
	local HOURS=${DELIM}$(( HOURS % 24 ))
	# fix zeros
	local DAYS=${DAYS:(-3)}
	local HOURS=${HOURS:(-2)}
	# return
	echo ${YEARS}y${DELIM}${DAYS}d${DELIM}${HOURS}h
}



cat /tmp/disks
echo
LSHW=$(lshw -short)
main "$@"
