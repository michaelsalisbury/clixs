#!/bin/bash



function main(){
	local pool=$1
	zfs list -r -o used,name -t snapshot ${pool} |
	while read used snap; do
		#if zfs destroy -vn "${snap}" | awk '{ if($2=="reclaim") exit $3==0?0:1 }'; then
		if [ "${used}" == "0" ] && zfs destroy -vn "${snap}" | awk '{ if($2=="reclaim") exit $3==0?0:1 }'; then
			zfs destroy -v "${snap}"
		else
			echo NOT EMPTY :: "${snap}"
		fi
	done
}
main "$@"
