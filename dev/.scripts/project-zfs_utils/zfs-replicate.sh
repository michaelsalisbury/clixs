#!/bin/bash
function main(){
	# veryify that dependant applications are installed
	which pv &> /dev/null || { echo ERROR :: command \"pv\" is not installed. Exiting\!; exit 1; }
	# 
	local update='true'
	local snapshot_id="zfs-replication-"
	local depth=${3:-0}
	local dst=$2
	local src=$1
	local src_snapshot dataset size 
	# verify src and dst
	for dataset in "${src}" "${dst}"; do
		zfs list -H -o type "${dataset}" |
		grep -q "^\(volume\|filesystem\)" ||
		{ echo ERROR :: \"${dataset}\" not a filesystem or volume. Exiting\!; exit 1; }
	done
	# get src type
	local src_type=$(zfs list -H -o type "${src}")
	# lock down destination
	if [ "${dst##*/}" != "${src##*/}" ]; then
		if zfs list -H -o type "${dst}/${src##*/}" 2>/dev/null | grep -q "^${src_type}$"; then
			dst="${dst}/${src##*/}"
		elif zfs list -H -o type "${dst}/${src#*/}" 2>/dev/null | grep -q "^${src_type}$"; then
			dst="${dst}/${src#*/}"
		elif zfs list -H -o type "${dst}/${src}" 2>/dev/null | grep -q "^${src_type}$"; then
			dst="${dst}/${src}"
		else
			dst="${dst}/${src##*/}"
			update=false
		fi
	fi

	if ${update}; then
		echo INFO\  :: update mode\; \"${src}\" \> \"${dst}\"
		_update	"${src}" "${dst}"
	else
		echo INFO\  :: copy mode\; \"${src}\" \> \"${dst}\"
		_copy	"${src}" "${dst}"
	fi

	if (( depth-- )); then
		zfs list -H -r -d 1 -o name "${src}" | tail -n +2 |
		while read src; do
			main "${src}" "${dst}" ${depth}
		done
	fi

}
function _update(){
	local creation name inc_snapshot size
	local src=$1
	local dst=$2

	#awk '{sub(/.*@/,"",$NF);print $1,$NF}' |
	read creation inc_snapshot < <(
		zfs list -t snapshot -r -d 1 -H -p -o creation,name "${src}" "${dst}" |
		awk -vSRC="${src}@" '{sub(/.*@/,SRC,$NF);print $1,$NF}' |
		sort -n |
		uniq -d |
		tail -1)

	# find most recent snapshot
	#while read creation name; do
	#	if zfs list -H -o type "${src}@${name##*@}" 2>/dev/null | grep -q "^snapshot$"; then
	#		inc_snapshot="${src}@${name##*@}"
	#		break	
	#	fi
	#done < <(zfs list -p -H -t snapshot -r -d 1 -o creation,name "${dst}" | sort -n -r)

	# if no src_snapshot was found then exit with error
	if [ -z "${inc_snapshot}" ]; then
		echo ERROR :: destination dataset \"${dst}\" exists but no matching snapshot can be found. Exiting\!
		exit 2
	fi

	# take snapshot
	name=$(zfs_snapshot)

	# replicate
	zfs_replication "${name}" "${dst}" "${inc_snapshot}"

	# clean old replication snapshots (this fails and cleans the wrong snapshots if no data is transfered due to no change)
	#zfs list -H -o name -t snapshot -r -d 1 "${src}" "${dst}" |
	#sed "\|${name##*@}$|d;\|@${snapshot_id}|!d" |
	#xargs -i@ zfs destroy -v "@"
}
function _copy(){
	local creation tmp_snapshot inc_snapshot size
	local src=$1
	local dst=$2

	# take snapshot
	if zfs list -H -p -o name -t snapshot -r -d 1 "${src}" | wc -l | (( `cat` )); then
		# snapshot exists
		echo INFO\  :: snapshot for \"${src}\" exists
		read creation inc_snapshot < <(
			zfs list -p -H -t snapshot -r -d 1 -o creation,name "${src}" |
			sort -n |
			sed -n 1p)
	else
		# take snapshot
		echo WARN\  :: snapshot for \"${src}\" does not exist, rectifying
		inc_snapshot=$(zfs_snapshot)	
	fi

	# replicate
	zfs_replication "${inc_snapshot}" "${dst}"

	# recurse
	_update "${src}" "${dst}"
}
function zfs_replicate(){
	local snapshot=$1
	local snapshot_destination=$2
	local snapshot_inc=$3
	local snapshot_size=$(zfs_snapshot_size "${snapshot}" "${snapshot_inc}")
	zfs send ${snapshot_inc:+-I} "${snapshot_inc}" "${snapshot}" |
		pv -b -p -t -r -s ${snapshot_size} |
		zfs receive "${snapshot_destination}"
}
function zfs_snapshot_size(){
	local snapshot=$1
	local snapshot_inc=$2
	zfs send -nv ${snapshot_inc:+-i} "${snapshot_inc}" "${snapshot}" 2>&1 |
	awk 'NR>1{gsub(/\.[0-9]*/,"",$NF); print $NF}')
}
function zfs_snapshot(){
	local snapshot_src=${src}
	local snapshot_desc=${snapshot_id}
	local snapshot="${snapshot_src}@${snapshot_desc}${RANDOM}"
	zfs snapshot "${snapshot}"
	local snapshot_time=$(zfs list -p -H -o creation "${snapshot}")
	zfs rename "${snapshot}" "${snapshot_src}@${snapshot_desc}${snapshot_time}"
	echo "${snapshot_src}@${snapshot_desc}${snapshot_time}"
}
main "$@"
