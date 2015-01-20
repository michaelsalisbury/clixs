#!/bin/bash

function main(){
	local _src_dev=$1
	local _dst_dev=$2
	local _parts=${3:-0}
	[ -b "${_src_dev}" ]             || EXIT_ERROR 1
	[ -b "${_dst_dev}" ]             || EXIT_ERROR 2
	

	part_report ${_src_dev}
	echo

	# clean destination disk
	sgdisk -Z ${_dst_dev}
	sgdisk -o ${_dst_dev}

	# copy table
	local      Sectors Number Start End Size m Code Name
	while read Sectors Number Start End Size m Code Name; do
			(( Number != 0 && Number > _parts )) && break
			echo ${Number}:${Size} ${m}:${Code}:${Name}
			sgdisk ${_dst_dev} -n ${Number}:+1M:+${Sectors}s
			sgdisk ${_dst_dev} -t ${Number}:${Code}
			[ -z "${Name}" ] && continue
			sgdisk ${_dst_dev} -c ${Number}:"${Name}"
	done < <(part_report ${_src_dev} | tail -n +2)

	echo
	part_report ${_dst_dev}
	echo

}
function part_report(){
	(( ${#1} )) && local _dev=$1 || EXIT_ERROR 1
	[ -b "${_dev}" ]             || EXIT_ERROR 2
	cat <<-AWK | awk -f <(cat) < <(sed -n '/^Number/,$ p' <(sgdisk -p ${_dev}))
		{
			FORMAT = "%10s %s\n"
			SECTORS = (\$3 - \$2 + 1)
			printf FORMAT, (SECTORS == 1)?"${_dev}":SECTORS, \$0
		}
	AWK
	return
}
function EXIT_ERROR(){
	
	exit $1
}

main "$@"

exit 0
sgdisk /dev/sdj -p | awk { DIF = ($3 - $2 + 1); printf "%10s %s\n", (DIF == 1)?"":DIF, $0}
