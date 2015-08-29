#!/bin/bash
function main(){
	(( ${#@} == 3 )) || { echo ERROR :: incorrect arg count. Exiting\!; exit 1; }
	local zvol=$1
	local part=${2}
	local path=${3}

	local zdev=$(readlink -f "/dev/zvol/${zvol}")
	local zpart=$(ls -1 "${zdev}"* | sed -n $(( part +1 ))p)

	_unmount "${zdev}"

	mkdir -p                     "${path}"
	mount "${zpart}"             "${path}"
	mkdir -p                     "${path}"/dev
	mount --bind /dev            "${path}"/dev
	mkdir -p                     "${path}"/run/resolvconf
	mount --bind /run/resolvconf "${path}"/run/resolvconf

	chroot "${path}" /bin/mkdir    -p /proc /sys
	chroot "${path}" /bin/mount    -t proc none /proc
	chroot "${path}" /bin/mount    -t sysfs sys /sys
	#chroot "${path}" /bin/hostname -F /etc/hostname
	
	echo Entering chroot \"${path}\"
	chroot "${path}"

	_unmount "${zdev}"

}
function _unmount(){
	local dev=$1
	get_mounts_from_dev "${dev}" |
	while read dev path x; do
		get_mounts_recursively_from_path "${path}" |
		while read dev path x; do
			path=$(echo -e "${path}")
			retry='3'
			while true; do
				if umount "${path}"; then
					echo Path Unmounted :: \"${path}\"
					break
				elif ! (( retry-- )); then
					echo ERROR :: path failed to unmount\; \"${path}\". Exiting\!
					exit 1
				fi
			done
		done
	done
}
function get_mounts_from_dev(){
	local dev=$1
	for dev in "${dev}"*; do
		cat <<-AWK | awk -f <(cat) /etc/mtab
			\$1=="${dev}"{print}
		AWK
	done
}
function get_mount_from_path(){
	local path=$1
	cat <<-AWK | awk -f <(cat) /etc/mtab
		\$2=="${path}"{print}
	AWK
}
function get_sub_mounts_from_path(){
	local path=$1
	cat <<-AWK | awk -f <(cat) /etc/mtab
		\$2~"^${path%/}/"{print}	
	AWK
}
function get_mounts_recursively_from_path(){
	local path=$1
	get_sub_mounts_from_path "${path}" |
	while read dev path x; do
		${FUNCNAME} "${path}"
	done
	get_mount_from_path "${path}"
}
main "$@"
