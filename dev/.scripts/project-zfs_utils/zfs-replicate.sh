#!/bin/bash
function main(){
	local update='true'
	local snapshot_id="zfs-replication-"
	local depth=${3:-0}
	local dst=$2
	local src=$1
	local src_type=$(zfs list -H -o type "${src}")
	local src_snapshot dataset size 
	# verify src and dst
	for dataset in "${src}" "${dst}"; do
		zfs list -H -o type "${dataset}" |
		grep -q "^\(volume\|filesystem\)" ||
		{ echo ERROR :: \"${dataset}\" not a filesystem or volume. Exiting\!; exit 1; }
	done
	# lock down destination
	if zfs list -H -o type "${dst}/${src##*/}" | grep -q "^${src_type}$"; then
		dst="${dst}/${src##*/}"
	elif zfs list -H -o type "${dst}/${src#*/}" | grep -q "^${src_type}$"; then
		dst="${dst}/${src#*/}"
	elif zfs list -H -o type "${dst}/${src}" | grep -q "^${src_type}$"; then
		dst="${dst}/${src}"
	else
		dst="${dst}/${src##*/}"
		update=false
	fi

	if ${update}; then
		_update	"${src}" "${dst}"
	else
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
		echo ERROR :: destination dataset exists but no matching snapshot can be found. Exiting\!
		exit 2
	fi

	# take snapshot
	name="${src}@${snapshot_id}${RANDOM}"
	zfs snapshot "${name}"
	creation=$(zfs list -p -H -o creation "${name}")
	zfs rename "${name}" "${src}@${snapshot_id}${creation}"
	name="${src}@${snapshot_id}${creation}"
	# get estimated send size
	size=$(zfs send -nv -i "${inc_snapshot}" "${name}" 2>&1 | awk 'NR>1{gsub(/\.[0-9]*/,"",$NF); print $NF}')
	# replicate
	zfs send -I "${inc_snapshot}" "${name}" |
	pv -b -p -t -r -s ${size} |
	zfs receive "${dst}"

	# clean old replication snapshots
	zfs list -H -o name -t snapshot -r -d 1 "${src}" "${dst}" |
	sed "\|${name##*@}$|d;\|@${snapshot_id}|!d" |
	xargs -i@ zfs destroy -v "@"
}
function _copy(){
	local creation name inc_snapshot size
	local src=$1
	local dst=$2

	# take snapshot
	if zfs list -H -p -o name -t snapshot -r -d 1 "${src}" | wc -l | (( `cat` )); then
		read creation inc_snapshot < <(
			zfs list -p -H -t snapshot -r -d 1 -o creation,name "${src}" |
			sort -n |
			sed -n 1p)
	else
		# take snapshot
		name="${src}@${snapshot_id}${RANDOM}"
		zfs snapshot "${name}"
		creation=$(zfs list -p -H -o creation "${name}")
		zfs rename "${name}" "${src}@${snapshot_id}${creation}"
		name="${src}@${snapshot_id}${creation}"
	fi

	# get estimated send size
	size=$(zfs send -nv "${inc_snapshot}" 2>&1 | awk 'NR>1{gsub(/\.[0-9]*/,"",$NF); print $NF}')
	# replicate
	zfs send "${inc_snapshot}" |
	pv -b -p -t -r -s ${size} |
	zfs receive "${dst}"
	# recurse
	_update "${src}" "${dst}"
}
main "$@"
