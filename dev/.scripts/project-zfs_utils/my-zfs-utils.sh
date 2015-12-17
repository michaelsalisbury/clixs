#!/bin/bash

function main(){
	if declare -F "do_${1}" &>/dev/null; then
		local cmd=$1
		shift
		do_$cmd "$@"
	else
		declare -F | sed -n 's/^declare -f do_//p'
	fi
}
function do_help(){
	declare -F | sed -n 's/^declare -f do_//p'
}
function do_dev(){
	get_booted_root_filesystem
}
function do_list_(){
	:
}
function do_list_boot_fs_dependents(){
	__list_boot_fs_dependents
}
function do_list_boot_fs(){
	get_boot_filesystems
}
function do_list_leaf_fs(){
	__list_chroot_fs_cache
}
function do_list_all_vol(){
	< <(__list_all_vol_cache) sort
}
function do_list_chroot_vol(){
	__list_chroot_vol
}
function do_grub_get_menuentries(){
	__grub_list_menuentries_raw
}
function do_grub_get_default(){
	[ -e "${__grub_dir}/grubenv" ] || return 1
	< <(< "${__grub_dir}/grubenv" sed -n 's/^saved_entry=//p') __sed_escape_string 1
}
function do_grub_set_default(){
	grub-set-default "${1}"
}
function do_grub_get_next_reboot(){
	[ -e "${__grub_dir}/grubenv" ] || return 1
	< <(< "${__grub_dir}/grubenv" sed -n 's/^next_entry=//p') __sed_escape_string 1
}
function do_grub_set_next_reboot(){
	grub-reboot "${1}"
}
function do_grub_run_menu_test(){
	__grub_run_menu_test
}
function do_grub_run_update(){
	update-grub
}
function do_remove_zfs_filesystem_empty_sub_folders(){
	local filesystem=$1
	is_zfs_filesystem "${filesystem}" || return 1
	get_zfs_mountpoint_folder_list "${filesystem}" |
		grep -v -x -f <(get_zfs_mountpoint) |
		xargs -i$'\x255' find "$'\x255'" -maxdepth 0 -type d -empty -exec rm -rfv '{}' ';'
}
function do_chroot_vol(){
	__chroot_vol "$@"
}
function do_chroot_boot_fs(){
	__chroot_zfs_filesystem "$@"
}
function do_chroot_leaf_fs(){
	while [[ "${1//[^[:alpha:]-]/_}" != +(--|) ]]; do
		case "${1}" in
			-n|--noask)		local chroot_mountpoint_noask='true';;
			-I|--ignore)	local chroot_mountpoint_noask='true'; local chroot_mountpoint_noask_reply='i';;
			-q|--quiet)		local chroot_mountpoint_quiet='true';;
			-P|--nopts)		local chroot_mountpoint_mnt_pts='false';;
			-R|--noroot)	local chroot_mountpoint_mnt_root='false';;
			-D|--noresolv)  local chroot_mountpoint_mnt_resolvconf='false';;
			-S|--nosync)	local chroot_mountpoint_mnt_syncpath='false';;
			-s|--syncpaths)	local chroot_mountpoint_syncpaths=$2; shift;;
			-u|--usrpaths)	local chroot_mountpoint_usrpaths=$2; shift;;
			-L|--testls)	local chroot_mountpoint_run='true'; shift;;
			-u)				local chroot_mountpoint_umount='false';;
			*)				false;;
		esac
		(( $? )) || { shift; continue; }
		if is_zfs_filesystem "${1}"; then
			local filesystem=${1}
		fi
		shift
	done
	[ "${1}" == "--" ] && shift
	if (( $# )); then 
		local chroot_mountpoint_run='true'
		local chroot_mountpoint_cmd=( "$@" )
		local chroot_mountpoint_umount='false'
	fi
	__chroot_fs "${filesystem}"
}
function do_mount_all_boot_fs(){
	__mount_zfs_filesystem
}
function do_mount_boot_fs(){
	(( ${#1} )) || exit 1
	__mount_zfs_filesystem "$@"
}
function do_mount_fs_tree(){
	mount_fs_tree "${@}"
}
function do_update_zfs_initrd_undo(){
	__update_zfs_initrd "$@"
}
function do_update_zfs_initrd(){
	__update_zfs_initrd "$@"
}
function do_clone_boot_fs(){
	__clone_boot_zfs_filesystem "$@"
}
function do_remove_boot_fs(){
	__remove_boot_fs "$@"
}
function do_remove_fs_tree(){
	__remove_fs_tree "$@"
}
function do_make_boot_vol(){
	__make_boot_vol "$@"
}
function do_make_boot_fs(){
	__make_boot_fs "$@"
}
function do_make_boot_fs_scripts(){
	__make_boot_fs_scripts "$@"
}
function do_add_zfs_filesystem_to_grub(){
	:
}
function is_zfs_volume(){
	local filesystem="${1#/dev/zvol/}"
	zfs list -t volume -H -o name "${filesystem}" &>/dev/null && return 0

	local zd_named_path="/dev/zvol/${filesystem}"
	local zd_real_path=$(readlink -f ${zd_named_path})
	local zd=$(basename "${zd_real_path}")
	local zd_sys_path=$(readlink -f /sys/class/block/${zd})

	# block device is not a partition
	echo part check
	[ -f "${zd_sys_path}/partition" ] || return 1

	local zd_sys_path_parent=${zd_sys_path%/*}
	local zd_parent=$(basename "${zd_sys_path_parent}")
	local zd_named_paths=$(find /dev/zvol -type l -exec echo -n '{} ' ';' -exec readlink -f '{}' ';')
	local zd_named_parent=$(awk -v zd_parent=${zd_parent} '$NF~zd_parent"$"{print $1}' <<< "${zd_named_paths}")

	# parent zfs filesystem could not be matched
	(( ${#zd_named_parent} )) || return 1

	filesystem=${zd_named_parent#/dev/zvol/}
	#echo ${zd_named_parent}
	#echo ${filesystem}

	# recurse
	${FUNCNAME} "${filesystem}"
}
function is_zfs_filesystem(){
	local filesystem=$1
	zfs list -t filesystem -H -o name "${filesystem%/}" &>/dev/null
}
function get_zfs_mountpoint(){
	local filesystem=$1
	if (( ${filesystem:+1} )); then
		zfs list -t filesystem -H -o mountpoint "${filesystem%/}" 2>/dev/null
	else
		zfs list -t filesystem -H -o mountpoint 2>/dev/null
	fi
}
function get_zfs_mountpoint_folder_list(){
	local filesystem=$1
	local mountpoint=$(get_zfs_mountpoint "${filesystem}")
	find "${mountpoint%/}" -maxdepth 1 -type d
}
function is_zfs_mountpoint_default(){
	local filesystem=$1
	zfs list -t filesystem -H -o mountpoint "${filesystem%/}" | grep -q "/${filesystem}"
}
function get_booted_root_filesystem(){
	mount | awk '$3=="/"{print $1}'
}
function get_system_kernels(){
	case "$(get_OS)" in
		Ubuntu)	ls -1 /boot/vmlinuz* 2>/dev/null;;
	esac
}
function get_OS(){
	if which lsb_release &>/dev/null; then
		lsb_release -s -i
	fi
}
function get_(){
	:
}
__make_boot_fs_scripts(){
	local filesystem=${1}
	local filesystem_base=${filesystem%/*/*/*}
	is_zfs_filesystem "${filesystem}" || return 1
	if ! is_zfs_mountpoint_default "${filesystem}"; then
		echo the boot filesystem \"${filesystem}\" is the mounted root, cancelling request to setup zfs_utils
		exit 1
	fi
	# ensure that the filesystem is mounted
	if ! is_mounted "${filesystem}" && ! mount_fs "${filesystem}"; then
			echo the filesystem \"${filesystem}\" does not seem to want to mount, cancelling request to setup zfs_utils
			exit 1
	fi
	# get mountpoint
	local mountpoint
	< <(get_mountinfo_entries "${filesystem}") read x mountpoint x x x x x
	echo filesystem \"${filesystem}\" mounted to path \"${mountpoint}\", proceeding to make scripts.
	
	cd "${mountpoint}"
	# /etc/grub.d/60_zfs
	if [ -f "etc/rc.local" ] && ! grep -q "^ *zfs mount -a" "etc/rc.local"; then
		chmod +x "etc/rc.local"
		local   TMP=$(<"etc/rc.local")
		local   END=$(< <(<<< "${TMP}" grep -n "^ *exit 0") cut -f1 -d:)
		local BEGIN=$(( END - 1 ))
		(	<<< "${TMP}" head -${START}
			cat <<-CAT

				# "${filesystem_base}" may need to be manually modified
				zfs list -H -o name -r "${filesystem_base}" |
				 	xargs -i@ zfs set mountpoint=/@ @ && true
				zfs mount -a && true

			CAT
			<<< "${TMP}" tail -n +${END}
		) > "etc/rc.local"
		echo rc.local at \"etc/rc.local\" is configured.  manual intervention should not be needed.
	else
		echo rc.local at \"etc/rc.local\" is already configured.  manual intervention needed if it is not mouting fs as expected.
	fi

	# /etc/grub.d/60_zfs
	if [ -d "etc" ]; then 
		mkdir "etc/grub.d" 2> /dev/null
		ln -s "/root/.bash_scripts.d/60_zfs" "etc/grub.d/". 2> /dev/null
		if (( $? )); then
			echo grub config \"etc/grub.d/60_zfs\" already linked to \"$(readlink -e "etc/grub/60_zfs")\".
		else
			echo grub config \"etc/grub.d/60_zfs\" linked to \"$(readlink -e "etc/grub/60_zfs")\", successful.
		fi
	fi

	# /etc/default/grub
	if [ -d "etc/default" ]; then 
		if [ -f "/root/.etc/default/grub" ]; then
			if [ -L "etc/default/grub" ]; then
				echo grub config \"etc/default/grub\" already linked to \"$(readlink -e "etc/default/grub")\".
			else
				if [ -f "etc/default/grub" ]; then
					mv "etc/default/grub"{,.bk}
					echo grub config \"etc/default/grub\" backed up to \"etc/default/grub.bk\".
				else
					echo grub config \"etc/default/grub\" does not exist so not backing up.
				fi
				ln -s "/root/.etc/default/grub" "etc/default/".
				echo grub config \"etc/default/grub\" linked to \"$(readlink -e "etc/default/grub")\", successful.
			fi
		else
			echo grub config target softlink missing from path \"/root/.etc/default/grub\", error creating soft link.
		fi
	fi

	# /etc/fstab
	if [ -d "etc" ]; then
		if is_mounted "/boot"; then
			local ID_FS_UUID ID_FS_UUID_ENC ID_FS_TYPE
			local ID_FS_DEV=$(< <(mount) sed -n 's|^\(.*\) on /boot type .*|\1|p')
			. <(blkid -o udev "${ID_FS_DEV}") # ID_FS_UUID ID_FS_UUID_ENC ID_FS_TYPE
			if ! [ -f "etc/fstab" ] || ! grep -q "^UUID=${ID_FS_UUID}[[:space:]]\+/boot[[:space:]]" "etc/fstab"; then
				echo UUID=${ID_FS_UUID} /boot ${ID_FS_TYPE} defaults 0 2 >> "etc/fstab"
				echo device UUID \"${ID_FS_UUID}\" configured in \"etc/fstab\" to mount to path \"/boot\".
			else
				echo device UUID \"${ID_FS_UUID}\" already configured in \"etc/fstab\" to mount to path \"/boot\".
			fi
		else
			echo device UUID not configured in \"etc/fstab\" to mount path \"/boot\", error path not discoverable.
		fi
	fi
}
__make_boot_vol(){
	__make_boot_ "$@"
	echo hi
}
__make_boot_fs(){
	__make_boot_ "$@"
}
__make_boot_(){
	local filesystem=${1}
	local distro=${2,,}
	local version=${3,,}
	local id=${4:-dev}
	local size=${5:-3G}
	local mountpoint=
	declare -F "${FUNCNAME}_${distro}" &>/dev/null || return 1
	# check if parent filesystem exists
	if ! is_zfs_filesystem "${filesystem}"; then
		echo zfs filesystem \"${filesystem}\" does not exist.  cancelling request to build fs.
		exit 1
	# make distro fs as required
	elif filesystem+="/${distro}" && ! is_zfs_filesystem "${filesystem}"; then
		zfs create "${filesystem}"
		zfs set atime=off "${filesystem}"
		zfs set compression=lz4 "${filesystem}"
		filesystem+="/${version}"
		zfs create "${filesystem}"
	# make version fs as required
	elif filesystem+="/${version}" && ! is_zfs_filesystem "${filesystem}"; then
		zfs create "${filesystem}"
		zfs set atime=off "${filesystem}"
		zfs set compression=lz4 "${filesystem}"
	# check to see if distro ID exists
	elif < <(zfs list -H -o name -t filesystem -r "${filesystem}" | grep -x "${filesystem}/${id}-.*" && echo -n $'\x255') read -d $'\x255'; then
		echo the target ID \"${id}\" already exists, cancelling request to build fs.
		echo
		echo List of revisions with the target ID.
		echo "${REPLY}"
		exit 1
	fi
	filesystem+="/${id}-0000-$(get_date_for_clone_filesystem_name)"
	case "${FUNCNAME[1]}" in
		__make_boot_fs)
			zfs create "${filesystem}"
			(( $? )) && echo target filesystem \"${filesystem}\" create error, exiting. && exit 1
			< <(get_mountinfo_entries "${filesystem}") read x mountpoint x x x x x
			;;

		__make_boot_vol)
			zfs create -o refreservation=none -V ${size} "${filesystem}"
			(( $? )) && echo target volume \"${filesystem}\" create error, exiting. && exit 2
			< <(file "$(readlink -e "/dev/zvol/${filesystem}")") grep -q "block special"
			(( $? )) && echo target volume \"${filesystem}\" device missing or not block special, exiting. && exit 3
			mkfs.ext4 "$(readlink -e "/dev/zvol/${filesystem}")"
			(( $? )) && echo target volume \"${filesystem}\" format error, exiting. && exit 4
			local ID_FS_UUID ID_FS_UUID_ENC ID_FS_TYPE
			. <(blkid -o udev "$(readlink -e "/dev/zvol/${filesystem}")")
			mkdir "/${filesystem}"
			mount -v -t ${ID_FS_TYPE} "$(readlink -e "/dev/zvol/${filesystem}")" "/${filesystem}"
			(( $? )) && echo target volume \"${filesystem}\" mount error, exiting. && exit 5
			< <(get_mountinfo_entries "$(readlink -e "/dev/zvol/${filesystem}")") read x mountpoint x x x x x
			;;
	esac

	${FUNCNAME}_${distro,,} "${mountpoint}" "${@:2}"

	case "${FUNCNAME[1]}" in
		__make_boot_fs)		_make_boot_fs_scripts "${filesystem}";;
		__make_boot_vol)	;;
	esac
}
__make_boot_fs_ubuntu(){
	local mountpoint=${1}
	local distro=${2}
	local version=${3}
	case "${version,,}" in
		trusty|utopic|vivid|wily|xenial);;
		14.04)	version='trusty';;
		14.10)  version='utopic';;
		15.04)  version='vivid';;
		15.10)  version='wily';;
		16.04)  version='xenial';;
		*)		false;;
	esac
	if (( $? )); then
		echo ${distro} version \"${version}\" not expected.  exiting.
		echo manually destroy filesystem or volume mounted to \"${mountpoint}\" to try again.
		exit 1
	elif which debootstrap &> /dev/null; then
		debootstrap ${version} "${mountpoint}"
	else
		echo please install debootstrap.  exiting.
		echo manually destroy filesystem or volume mounted to \"${mountpoint}\" to try again.
		echo 2
	fi
}
__grub_run_menu_test(){
	local R="\e[32m" r="\e[39m"
	echo -n -e "${R}"
	echo You are about to enter a simulated grub menu.
	echo To exit this menu enter the grub command line,
   	echo press \"c\" and then type \"reboot\".  This will
	echo not reboot your computer.  This simulation is
	echo done via the grub package \"grub-emu\".
	echo
	echo -n Press \"q\" to cancel or any key to start \"grub-emu\"
	echo -n -e "${r}"
	read -s -n 1 -t 3
	echo
	[[ "${REPLY}" =~ ^($'\e'|q|Q|c|C|n|N)$ ]] || grub-emu
}

__sed_escape_string () {
	local count=${1:-1}
	local sed_special_chars=$' >'
	local sed_escape_chars=$'\\\\'
	local sed_escape_chars=$(count=${count//[^[:digit:]]/} < <(printf %$(( count * 2 ))s) sed "s/./${sed_escape_chars}/g")
	sed "s/\([${sed_special_chars}]\)/${sed_escape_chars}\1/g"

}
__grub_list_menuentries_raw () {
	cat <<-AWK | awk -F "['\"]" -f <(cat) /boot/grub/grub.cfg | __sed_escape_string
		/^[ \t]*menuentry[ \t]/{
			menuentry=\$2

			if (submenu_l)
				for (submenu_i = 1; submenu_i <= submenu_l; submenu_i++)
					printf submenus[submenu_i]">"
			print menuentry
		}
		/^[ \t]*submenu[ \t]/{
			submenu_l++
			submenus[submenu_l]=\$2
		}
		/}[ \t]*$/{
			if (menuentry) {
				menuentry=""
			} else if (submenu_l) {
				delete submenus[submenu_l]
				submenu_l--
			}
		}
	AWK
}
__verify_zfs_grub_stor () {
	local filesystem=$1
	if ! is_zfs_filesystem "${filesystem}"; then
		cat <<-ERROR 1>&2
			${filesystem}\" is not a zfs filesystem.
		ERROR
	elif ! [ -d "${zfs_grub_base}/${filesystem}" ]; then
		cat <<-ERROR 1>&2
			The zfs grub stor "${zfs_grub_base}/${filesystem}" for filesystem "${filesystem}" does not exist.
			If the filesystem is boot please use the following commands to ammend and update grub.
			# $0 add_zfs_filesystem_to_grub ${filesystem}
			# $0 update_grub
		ERROR
	else
		return 0
	fi
		return 1
}
__update_zfs_initrd () {
	:
	local filesystem=${1:-$(get_booted_root_filesystem)}
	__verify_zfs_grub_stor "${filesystem}" || return 1
	local zfs_stor="${zfs_grub_base}/${filesystem}"
	local OS=$(get_OS)
	local KERNEL KERNEL_zfs INITRD INITRD_zfs INITRD_vers
	while read KERNEL; do
		case "${OS}" in
			Ubuntu)
				INITRD=$(basename "${KERNEL}")
				INITRD=$(dirname "${KERNEL}")"/${INITRD/vmlinuz-/initrd.img-}"
			;;
		esac
		# set vars
		KERNEL_zfs=$(basename "${KERNEL}")
		INITRD_zfs=$(basename "${INITRD}")
		KERNEL_zfs="${zfs_stor}/${KERNEL_zfs}"
		INITRD_zfs="${zfs_stor}/${INITRD_zfs}"
		
		if [ -d "${zfs_stor}" ] && [ -f "${KERNEL}" ] && [ -f "${INITRD}" ]; then
			# progress/status
			echo -e Processing... \\t KERNEL :: ${KERNEL} \\t INITRD :: ${INITRD}
			case "${FUNCNAME[1]}" in
				do_update_zfs_initrd_undo)
					# test if initrd.img is not new and remove accordingly
					if ! [ -f "${INITRD_zfs}" ]; then
						echo INITRD \"${INITRD_zfs}\" not found so not testing for removal.
					elif diff "${INITRD}" "${INITRD_zfs}" &> /dev/null; then
						rm -vf "${INITRD_zfs}"
						ls -1 "${INITRD_zfs}".v* 2>/dev/null |
							tail -1 |
							xargs -i$'\x255' mv -v "$'\x255'" "${INITRD_zfs}"
					else
						echo INITRD \"${INITRD_zfs}\" differs from \"${INITRD}\" so not removing.
					fi
					;;
				do_update_zfs_initrd)
					# hard link kernel
					if ! [ -f "${KERNEL_zfs}" ]; then
						echo linking ${KERNEL} to ${zfs_stor}
						ln -f "${KERNEL}" "${zfs_stor}/".
					else
						echo KERNEL not updated.
					fi
					# hard link initrd, preserve existing/workig versions
					if ! [ -f "${INITRD_zfs}" ]; then
						echo linking ${INITRD} to ${zfs_stor}
						ln -f "${INITRD}"  "${zfs_stor}/".
					# test if initrd.img is new and increment accordingly
					elif ! diff "${INITRD}" "${INITRD_zfs}" &> /dev/null; then
						INITRD_vers=$(ls -1 "${INITRD_zfs}".v* 2>/dev/null | tail -1)
						INITRD_vers=${INITRD_vers: -2}
						INITRD_vers=0$(( ${INITRD_vers:- -1} + 1 ))
						INITRD_vers=${INITRD_vers: -2}
						mv -v "${INITRD_zfs}"{,.v${INITRD_vers}}
						echo linking ${INITRD} to ${zfs_stor}
						ln -f "${INITRD}"  "${zfs_stor}/".
					else
						echo INITRD not updated.
					fi
					;;
			esac
		fi
	done < <(get_system_kernels)
}
__vet_zfs_base () {
	local filesystem=$1
	if ! is_zfs_filesystem "${filesystem}"; then
		if ! is_zfs_filesystem "${filesystem%%/*}"; then
			echo zfs pool \"${filesystem%%/*}\" does not exists.
			echo new base zfs filesystem for clone cannot be created, cancelling request for clone.
			exit 1
		elif ! is_zfs_filesystem "${filesystem%/*}"; then
			echo zfs parent filesystem \"${filesystem%%/*}\" does not exist.
			echo new base zfs filesystem for clone cannot be created, cancelling request for clone.
			exit 1
		else
			zfs create "${filesystem}"
			echo new base zfs filesystem created... "${filesystem}"
		fi
	else
		# ensure we didn't choose the booted filesystem
		if get_booted_root_filesystem | grep -q -x "${filesystem}"; then
			echo selected base zfs filesystem \"${filesystem}\" is the current mounted root/boot filesystem.
			echo nesting a cloned root/boot filesystem here is ill-advised, cancelling request for clone.
			exit 1
		fi
		# the base zfs filesystem that containes root/boot filesystems should be mounted in it's default location
		if ! is_zfs_mountpoint_default "${filesystem}"; then
			echo selected base zfs filesystem \"${filesystem}\" is not mounted in it\'s default location.
			echo although this is not a technical problem it may indicate that the selected filesystem is not appropriate, cancelling request for clone.
			exit 1
		fi
		# the base zfs filesystem that containes root/boot filesystems should probably not contain any standard folders that are not also zfs filesystems
		# files for in-system documentation are to be expected.
		if get_zfs_mountpoint_folder_list "${filesystem}" | grep -q -v -x -f <(get_zfs_mountpoint); then
			echo selected base zfs filesystem \"${filesystem}\" contains unix folders that are not filesystems.
			echo if they are empty please remove them with the command \"$0 remove_zfs_filesystem_empty_sub_folders\" and try to clone again.
			echo if not then this filesystem may not be appropriate, cancelling request for clone.
			exit 1
		fi
		# the relative grub stor for the base zfs filesystem that containes root/boot grub stors should probably be
		# either empty or contain sub-folders.  if it contains files and no folders it may be root/boot grub stor
		if [ -d "${zfs_grub_base}/${filesystem%/}" ] &&
			 find "${zfs_grub_base}/${filesystem%/}" -maxdepth 1 -type f | read &&
			 ! find "${zfs_grub_base}/${filesystem%/}" -maxdepth 1 -mindepth 1 -type d | read; then
			echo new base zfs filesystem \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" contains files and no directories.
			echo please investigate this filesystem and the related grub stor to be sure it is not a root/boot filesystem.
			echo if you would like to proceed please empty the directory \"${zfs_grub_base}/${filesystem%/}\" of files and try again, cancelling request for clone.
			exit 1
		fi
		# the relative grub stor for the base zfs filesystem that containes root/boot grub stors should be a directory
		if [ -e "${zfs_grub_base}/${filesystem%/}" ] && ! [ -d "${zfs_grub_root}/${filesystem%/}" ]; then
			echo new base zfs \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" path exists but is not a directory, cancelling request for clone.
			exit 1
		fi
		# the relative grub stor for the base zfs filesystem that containes root/boot grub stors needs to be created
		if ! [ -e "${zfs_grub_base}/${filesystem%/}" ]; then
			echo creating zfs base grub stor \"${zfs_grub_base}/${filesystem%/}\".
			mkdir -p "${zfs_grub_base}/${filesystem%/}"
		else
			echo new base zfs filesystem \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" exists and contains no files.
			echo this probably means that the request new base zfs filesystem will work as expected as a container for root/boot filesystems.
		fi
	fi
}
function get_boot_filesystems(){
	find "${zfs_grub_base}" -type d |
		tac |
		awk 'LAST~"^"$0"/"{next}{print;LAST=$0}' |
		sed "s|^${zfs_grub_base}/||" |
		xargs -i"$'\255'" zfs list -H -o name "$'\255'" 2>/dev/null
}
__mount_zfs_filesystem () {
	local filesystem=$1
	#if get_booted_root_filesystem | grep -q -x "${filesystem}"; then
	#	echo the boot filesystem \"${filesystem}\" is the mounted root, cancelling request for mount.
	#	exit 1
	#fi
	if is_zfs_filesystem "${filesystem}"; then
		echo "${filesystem}"
	elif ! (( ${filesystem:+1} )); then
		get_boot_filesystems
	else
		exit 1
	fi |
	while read filesystem; do
		mountpoint=$(zfs list -H -o mountpoint "${filesystem}")
		if get_booted_root_filesystem | grep -q -x "${filesystem}"; then
			echo the boot filesystem \"${filesystem}\" is the mounted root, skipping mount request.
		elif zfs list -H -o mounted "${filesystem}" | grep -q -x "yes"; then
			echo the boot filesystem \"${filesystem}\" is already mounted to \"${mountpoint}\", skipping mount request.
		elif [ "${mountpoint}" == "/" ]; then
			echo the boot filesystem \"${filesystem}\" mountpoint was \"${mountpoint}\", reseting to default and attempting mount.
			zfs set mountpoint="${filesystem}" "${filesystem}"
			zfs mount "${filesystem}"
		else
			echo the boot filesystem \"${filesystem}\" mountpoint is \"${mountpoint}\" but was not mounted, attempting mount.
			zfs mount "${filesystem}"
		fi
	done
	
}

__list_chroot_fs_cache(){
	get_cache 1
}
__list_chroot_fs(){
	# this will list only the filesystem (and not volumes) that have no children
	< <(
		while read name; do
			zfs list -r -H -o name -t filesystem "${name}" 2>/dev/null &
		done < <(
			while read name; do
				zfs list -r -H -o name "${name}" |
				tac |
				awk 'LAST~"^"$0"/"{next}{print;LAST=$0}' |
				tac &
			done < <(
				zpool list -H -o name
			)
		)
	) sort
}

__list_all_vol_cache(){
	get_cache __list_all_vol_details 2
}
__list_chroot_vol(){
	< <(< <(__list_all_vol_cache) sort) grep "[[:space:]]ext[234][[:space:]]"
}
__list_all_vol_details(){
	local      zfs_vols=$(find /dev/zvol -type l)
	
	local -A ID_PART_ENTRY_TYPES
	local                            ID_PART_ENTRY_NAME     ID_PART_ENTRY_TYPE
	while read                       ID_PART_ENTRY_TYPE     ID_PART_ENTRY_NAME; do
		     ID_PART_ENTRY_TYPES[0x${ID_PART_ENTRY_TYPE}]=${ID_PART_ENTRY_NAME}
	done < <(sfdisk -T)

	local    fd_MEM_IN fd_MEM_OUT
	randomfd fd_MEM_IN fd_MEM_OUT

	(
		fd_MEM_ARY=()
		arg_ARY=()
		while read -u ${fd_MEM_OUT} -a REPLY; do
			(( ${REPLY[1]} )) || break
			fd_MEM_ARY[${#fd_MEM_ARY[*]}]="/proc/${REPLY[0]}/fd/${REPLY[1]}"
			read -u ${fd_MEM_IN} -a REPLY
			for i in ${!REPLY[*]}; do
				(( ${REPLY[$i]} > ${arg_ARY[$i]:-0} )) && arg_ARY[$i]=${REPLY[$i]}
			done
		done

		for i in ${!fd_MEM_ARY[*]}; do
			echo ${arg_ARY[*]} >& ${fd_MEM_ARY[$i]} &
		done
	) &
	while read zd_named_path; do
		(
			randomfd          fd_SUB
			echo ${BASHPID} ${fd_SUB} $(date +%s.%N) >& ${fd_MEM_OUT}

			. <(__get_vol_details ${zd_named_path})
			FS=$(__get_vol_fs)

			echo            ${#ZFS}      ${#FS}     ${#ZD_SIZE_G}          ${#ZD}      ${#ZPOOL}     ${#UUID} >& ${fd_MEM_IN}
			read -u ${fd_SUB}  ZFS_l        FS_l       ZD_SIZE_G_l            ZD_l        ZPOOL_l       UUID_l
			printf        "%-${ZFS_l}s  %-${FS_l}s %'${ZD_SIZE_G_l}.3f GB  %-${ZD_l}s  %${ZPOOL_l}s  %-${UUID_l}s\n"\
                            "${ZFS}"      ${FS}      ${ZD_SIZE_G}            ${ZD}      ${ZPOOL}       ${UUID}
		) &
	done <<< "${zfs_vols}"

	sleep 1
	echo ${BASHPID} 0 $(date +%s.%N) >& ${fd_MEM_OUT}

	wait
}
__get_vol_fs(){
	if ${ISEXTFS}; then
		local FS=${FILE[4]}
	elif ${ISSWAP}; then
		local FS='SWAP'
	elif ${ISLVM2}; then
		local FS='lvm2'
	elif ${ISDATA}; then
		if (( ${#ID_PART_ENTRY_TYPE} )); then
			local FS=${ID_PART_ENTRY_TYPES[${ID_PART_ENTRY_TYPE}]}
		else
			local FS='data'
		fi
	else
		if (( ${#ID_FS_TYPE} )); then
			local FS=${ID_FS_TYPE}
		elif (( ${#ID_PART_ENTRY_TYPE} )); then
			local FS=${ID_PART_ENTRY_TYPES[${ID_PART_ENTRY_TYPE}]}
		elif (( ${#ID_PART_TABLE_TYPE} )); then
			local FS=${ID_PART_TABLE_TYPE}
		else
			local FS='unknown'
		fi
	fi
	echo "${FS}"
}
__get_vol_details(){
	local zd_named_path=$1
	local ZFS=${zd_named_path#/dev/zvol/}
	local ZPOOL=${ZFS%%/*}
	local ZD_REAL_PATH=$(readlink -f ${zd_named_path})
	local ZD=$(basename ${ZD_REAL_PATH})
	local ZD_SYS_PATH=$(readlink -f /sys/class/block/${ZD})
	local ZD_SYS_SIZE512=$(<"${ZD_SYS_PATH}/size")
	blkid -po udev "/dev/${ZD}" &
	echo ZD_SIZE_K=$(bc <<< "scale=3; ${ZD_SYS_SIZE512} / 2") &
	echo ZD_SIZE_M=$(bc <<< "scale=3; ${ZD_SYS_SIZE512} / 2000") &
	echo ZD_SIZE_G=$(bc <<< "scale=3; ${ZD_SYS_SIZE512} / 2000000") &
	local   FILE
	read -a FILE < <(< <(file -sL ${zd_named_path}) sed 's/UUID: /UUID=/')
	declare -p FILE ZFS ZPOOL ${!ZD*} &

	[ "boot sector" == "${FILE[*]:2:2}" ]   && echo ISBOOT=true             || echo ISBOOT=false
	[ "filesystem"  == "${FILE[5]}"     ]   && echo ISEXTFS=true            || echo ISEXTFS=false
	[ "swap file"   == "${FILE[*]:2:2}" ]   && echo ISSWAP=true             || echo ISSWAP=false
	[ "LVM2 PV"     == "${FILE[*]:1:2}" ]   && echo ISLVM2=true             || echo ISLVM2=false
	[ "data"        == "${FILE[*]:1}"   ]   && echo ISDATA=true             || echo ISDATA=false
	[[ "${FILE[@]:1}" =~ (UUID=)([^, ]+) ]] && echo UUID=${BASH_REMATCH[2]} || echo UUID=NA
}

__chroot_vol () {
	echo start1
	local filesystem="/dev/zvol/${1#/dev/zvol/}"
	local mountpoint="/mnt/zvol/${1#/dev/zvol/}"
	local umount_delay='1'

	is_zfs_volume "${filesystem}" || return 1

	# setup mount point	
	[ -d "${mountpoint}" ] || mkdir -p "${mountpoint}"


	local ID_FS_UUID ID_FS_UUID_ENC ID_FS_TYPE
	. <(blkid -o udev ${filesystem})

	# mount
	echo mounting root
	mount -v -t ${ID_FS_TYPE} "${filesystem}" "${mountpoint}"
	# mount failed
	(( $? )) && return 1
	
	__chroot_mountpoint "${mountpoint}"

	echo unmounting root
	umount -v "${mountpoint}"
	umount -v "${mountpoint}"
	#echo umount \""${mountpoint}"\" | at -q c now + ${umount_delay} minutes 2>/dev/null
}

__chroot_mountpoint () {
	local   mountpoint=$1	
	local        quiet=${chroot_mountpoint_quiet:-false}
	local        noask=${chroot_mountpoint_noask:-false}
	${quiet} &&  noask='true'
	local  noask_reply=${chroot_mountpoint_noask_reply:-u}
	local       umount=${chroot_mountpoint_umount:-true}
	local   chroot_run=${chroot_mountpoint_run:-false}
	local   chroot_cmd=( "${chroot_mountpoint_cmd[@]}" )
	local      mnt_pts=${chroot_mountpoint_mnt_pts:-true}
	local     mnt_root=${chroot_mountpoint_mnt_root:-true}
	local  mnt_rslvcnf=${chroot_mountpoint_mnt_resolvconf:-true}
	local mnt_syncpath=${chroot_mountpoint_mnt_syncpath:-true}
	local          IFS=":"
	local     usrpaths=( ${chroot_mountpoint_usrpaths} )  # colon seperated list of folder paths to bind mount to chroot fs
	local    syncpaths=( ${chroot_mountpoint_syncpaths} ) # colon seperated list of files and folder to unpdate sync to tmpfs
	unset          IFS
	local     syncpath=${chroot_mountpoint_syncpath:-"/tmp${mountpoint}"}
	local        paths=( /dev /proc /sys )
	# additional sys-defined paths to bind mount
	${mnt_pts}      && paths[${#paths[*]}]="/dev/pts"
	${mnt_root}     && paths[${#paths[*]}]="/root"
	${mnt_rslvcnf}  && paths[${#paths[*]}]="/run/resolvconf"
	${mnt_syncpath} && paths[${#paths[*]}]="${syncpath}"
	# additional user-defined paths to bind mount 
	(( ${#usrpaths[*]} )) && paths=( "${paths[@]}" "${usrpaths[@]}" )
	# default chroot cmd if none supplied
	(( ${#chroot_cmd[*]} )) || chroot_cmd=( ls -l / )
	
	# http://askubuntu.com/questions/528928/how-to-do-underline-bold-italic-strikethrough-color-background-and-size-i
	local U="\e[4m" u="\e[24m"
	local B="\e[1m" b="\e[22m"
	local -A mnts       devs    
	local    mnt  path  dev  fill sep i

	${mnt_syncpath} && {
		# create tmpfs
		mkdir -p "${syncpath}" &> /dev/null

		# syncing; you are responsible for removing syncpath and reverse syncing any changes
		if (( ${#syncpaths[*]} )); then
			${quiet} || echo you are responsible for removing tmpfs \"${syncpath}\" and reverse syncing any changes
			${quiet} || echo
			for path in "${syncpaths[@]}"; do (( ${#path} > ${#sep} )) && sep=${path//?/ }; done
			for path in "${syncpaths[@]}"; do
				fill=${sep:${#path}}
				local real=$(readlink -e "${path}" 2> /dev/null)
				if [ -d "${real}" ] || [ -f "${real}" ]; then
					local destination=$(dirname "${tmpfs}/${path#/}")
					${quiet} || echo -n path \""${path}"\" "${fill}" is syncing to \"${destination}\"\ 
					mkdir -p "${destination}" &> /dev/null
					${quiet} || trap_wait 3 . SIGUSR2 &
					rsync -vaxEAHPhu "${real}" "${destination}" &> /dev/null
					${quiet} || kill -USR2 $!
					${quiet} || echo
				else
					${quiet} || echo path \""${path}"\" "${fill}" is not a directory, regular file or link to either, skipping tmpfs sync.
				fi
			done
		fi
	}

	# set fpath (fill path) to length in spaces of longest path
	for path in "${paths[@]}"; do (( ${#path} > ${#sep} )) && sep=${path//?/ }; done

	# get mount and device names for target paths
	while read mnt path x x x dev x; do
		mnts[${path}]=${mnt}
		devs[${path}]=${dev}
		(( ${#dev} + ${#mnt} > ${#sep} ))  && sep="${dev//?/ }${mnt//?/ }"
	done < <(get_mountinfo_path_entries "${paths[@]}")

	# preping
	${quiet} || echo preping
	for i in $(< <(printf "%s\n" ${!paths[*]}) sort -nr); do
		path=${paths[i]}
		while true; do
			if is_mounted "${mountpoint}${path}"; then
				< <(get_mountinfo_path_entries "${mountpoint}${path}") read mnt x x x x dev x
				${quiet} || echo -n -e path \"${mountpoint}${path}\" "${sep:${#path}}" already bound ${dev}:${mnt} "${sep:${#dev}+${#mnt}}"
				if [ "${mnt}" == "${mnts[${path}]}" ] && [ "${dev}" == "${devs[${path}]}" ];then
					${quiet} || echo \ proceeding.
					break
				fi
				${quiet} || echo -n -e \ "[ ${U}u${u}mount|${U}i${u}gnore|${B}${U}c${u}ancel${b} ] >"
				${quiet} || ${noask} && REPLY=${noask_reply}
				${noask} || read -s -t 5 -n 1 -i c
				${quiet} || case "${REPLY,,}" in
					c)	echo    \ cancelling request for chroot.;;
					i)	echo    \ ignoring.;;
					u)	echo -n \ umount..;;
					*)	echo;;
				esac
				case "${REPLY,,}" in
					c)	return;;
					i)	break;;
					u)	if umount "${mountpoint}${path}" &> /dev/null; then
							${quiet} || echo success.
						else
							${quiet} && return 102 || echo error.
						fi;;
				esac
			elif ! [ -d "${mountpoint}${path}" ]; then
				${quiet} || echo -n -e path \"${mountpoint}${path}\" "${sep:${#path}}" for mount ${path} "${sep:${#path}}" does not exist,
				${quiet} || echo -n -e \ "[ ${U}m${u}kdir|${U}i${u}gnore|${B}${U}c${u}ancel${b} ] >"
				${quiet} || ${noask} && REPLY='m'
				${noask} ||	read -s -t 5 -n 1 -i c
				${quiet} ||	case "${REPLY,,}" in
					c)	echo    \ cancelling request for chroot.;;
					i)	echo    \ ignoring.;;
					m)	echo -n \ mkdir..;;
					*)	echo;;
				esac
				case "${REPLY,,}" in
					c)	return;;
					i)	break;;	
					m)	if mkdir -p "${mountpoint}${path}" &> /dev/null; then
							${quiet} || echo success.
						else
							${quiet} && return 102 || echo error.
						fi;;
				esac
			else
				${quiet} || echo -e path \"${mountpoint}${path}\" "${sep:${#path}}" ready to bind ${path} \ "${sep:${#path}}" proceeding.
				break
			fi
		done
	done

	# mounting 
	${quiet} || echo mounting
	for path in "${paths[@]}"; do
		while true; do
			if is_mounted "${mountpoint}${path}"; then
				< <(get_mountinfo_path_entries "${mountpoint}${path}") read mnt _path x x x dev x
				${quiet} || echo -n -e path \"${mountpoint}${path}\" "${sep:${#path}}" already bound ${dev}:${mnt} "${sep:${#dev}+${#mnt}}"
				if [ "${mnt}" == "${mnts[${path}]}" ] && [ "${dev}" == "${devs[${path}]}" ];then
					${quiet} || echo \ proceeding.
					break
				else
					${quiet} || echo -n -e \ "[ ${B}${U}u${u}nmount${b}|${U}i${u}gnore ] >"
					${quiet} || ${noask} && REPLY=${noask_reply}
					${noask} || read -s -t 5 -n 1 -i u
					${quiet} || case "${REPLY,,}" in
						i)	echo    \ ignoring.;;
						u)	echo -n \ umount..;;
						*)	echo;;
					esac
					case "${REPLY,,}" in
						i)	break;;
						u)	if umount "${mountpoint}${path}" &> /dev/null; then
								${quiet} || echo success.
							else
								${quiet} && return 103 || echo error.
							fi;;
					esac
				fi
			else
				${quiet} || echo -n -e path \"${mountpoint}${path}\" "${sep:${#path}}" bind mounting ${path} \ "${sep:${#path}}"
				if mount --bind "${path}" "${mountpoint}${path}" &> /dev/null; then
					${quiet} || echo \ success.
					break
				else
					${quiet} && return 105
					echo -e \ "${B}failed${b}", chroot is proceeding, be advised.
					break
				fi
			fi
		done
	done
	# chroot
	${quiet} || echo
	${quiet} || echo -n chrooting to \"${mountpoint}\" ...
	# do not remove the following 2 echos, __chroot__00 does a weird line shift up thing
	if ${chroot_run}; then
		${quiet} || echo \ executing cmd: "${chroot_cmd[@]}"
		chroot "${mountpoint}" "${chroot_cmd[@]}"
	else
		${quiet} || echo
		echo
		echo
		__chroot__00
		#chroot "${mountpoint}" /bin/bash -c "su -"
		unset debian_chroot
	fi

	# umount chroot mountpoint
	if ${umount}; then
		if ${quiet}; then
			unset quiet
			local chroot_mountpoint_unmount_noask='true'
			__chroot_mountpoint_umount "${mountpoint}" &> /dev/null &
		else
			local chroot_mountpoint_unmount_noask=${chroot_mountpoint_noask}
			__chroot_mountpoint_umount "${mountpoint}"
		fi
	fi
}
__chroot_mountpoint_umount(){
	local mountpoint=${mountpoint:-${1}}
	local quiet=${chroot_mountpoint_unmount_quiet:-${chroot_mountpoint_quiet:-${quiet:-false}}}
	local noask=${chroot_mountpoint_unmount_noask:-${chroot_mountpoint_noask:-${noask:-false}}}
	${quiet} && noask='true'

	# check for blocking processes; aka another chroot to same path
	local    fd_LSOF
	randomfd fd_LSOF

	while true; do
		${quiet} || echo -n scanning for open files within mountpoint \"${mountpoint}\"
		# display progress while lsof searches
		${quiet} || trap_wait 3 . SIGUSR2 &
		< <(lsof -F +D "${mountpoint}" 2>/dev/null) sed -n 's/^p//p' >& ${fd_LSOF}
		${quiet} || kill -USR2 $!
		# flushfd reports false if the fd is empty, aka the lsof command returned null
		# flushfd dumps the contents of the fd into REPLY[fd]
		if flushfd fd_LSOF; then
			local blocking_pids=${REPLY[fd_LSOF]}
			      blocking_pids+=$(ps --no-heading -o ppid -p ${blocking_pids//\n/ })
			${quiet} || ${noask} && REPLY='s'
			${quiet} || {
				echo listing blocking processes\;
				echo
				< <(ps -f -p ${blocking_pids//\n/ }) sed 's/^/\t/'
				echo
				echo it is very likelly that these will block the umount of the chroot environment,
				echo or the umount will crash the for listed processes.
				echo
			}
			${noask} || {
				echo -n -e "do you want to [ ${U}k${u}ill the processes|${B}${U}s${u}kip the umount${b}|${U}p${u}roceed anyway ] > "
				trap_wait 3 . SIGUSR1 &
				echo -n \ 
				read -t 9 -s -n 1 -i s
				kill -USR1 $! &> /dev/null
			}
			${quiet} || case "${REPLY}" in
				k)	echo killing processes.;;
				p)	echo proceeding to umount chroot.;;
				*)	echo skipping umount.;;
			esac
			case "${REPLY}" in
				k)	kill ${blocking_pids//\n/ };;
				p)	break;;
				*)	return;;
			esac
		else
			${quiet} || echo proceeding to umount chroot.
			break
		fi
	done

	local      err path sep
	while read err path sep; do
		${quiet} || {
			echo -n -e path \"${path}\" "${sep//?/ }"
			(( err )) && echo \ umount failed, be advised. || echo \ umount was successfull.
		}
	done < <(umount_subtree "${mountpoint}")
}
__value_is_number(){
	local VALUE=${1//[\$\`]/}
	(( ${#VALUE} )) || return 1
	[[ "${VALUE}" =~ (^[[:digit:]]+$)|(^[[:digit:]]*[.][[:digit:]]*$) ]] || return 1
	[[ "${VALUE}" =~ ^[.]$ ]] && return 1
	return 0
}
function trap_wait(){
	local int=${trap_wait_interval:-1}
	local chr=${trap_wait_char:-.}
	local sig=${trap_wait_sig:-SIGUSR1}
	local arg 
	for   arg in "$@"; do
		if __value_is_number "${arg}"; then
			int=${arg}
		elif [ "${arg:0:3}" == "SIG" ]; then
			sig=${arg}
		else
			chr=${arg:0:1}
		fi
	done

	trap 'break' ${sig}
	while true; do
		echo -n "${chr}"
		sleep ${int}
	done
}
__chroot_fs () {
	local filesystem=$1
	is_zfs_filesystem "${filesystem}" || return 1
	if ! is_zfs_mountpoint_default "${filesystem}"; then
		echo the boot filesystem \"${filesystem}\" is the mounted root, cancelling request for chroot.
		exit 1
	fi
	# ensure that the filesystem is mounted
	if ! is_mounted "${filesystem}" && ! mount_fs "${filesystem}"; then
			echo the filesystem \"${filesystem}\" does not seem to want to mount, cancelling request for chroot.
			exit 1
	fi
	# get mountpoint
	local mountpoint
	< <(get_mountinfo_entries "${filesystem}") read x mountpoint x x x x x
	# chroot mountpoint
	__chroot_mountpoint "${mountpoint}"
}
function __chroot__00(){
	local erase=$'\e[1A\e[s\e[K\e[m'
	local   red='\e[0;31m'
	local green='\e[0;32m'
	local  blue='\e[0;34m'
	local   clr='\e[0m'
	local   PS1="(${red}${filesystem##*/}${clr})${green}\u${clr}@${green}\h${clr}:${blue}\w${clr}\$ "

			#echo -n \$'\e[2A\e[s\e[K\e[m'
	chroot "${mountpoint}" /bin/su - <<-CHROOT
		# since we can't supress "stdin: is not a tty" error, the following ANSI escape sequences cursor movement erases it
		echo -n $'\e[1A\e[s\e[K\e[m'
		# commands within the interactive bash shell seem to echo so we use follow-up cursor movements to erase them as well
		/bin/bash -i -l <<-SHELL
			 echo -n $'\e[1A\e[s\e[K\e[m\e[1A\e[s\e[K\e[m'
			 export PS1="${PS1}"
			 echo -n $'\e[1A\e[s\e[K\e[m\e[1A\e[s\e[K\e[m'
			 export TEST=1234
			 echo -n $'\e[1A\e[s\e[K\e[m\e[1A\e[s\e[K\e[m'
			 exec </dev/tty
		SHELL
	CHROOT
			#exec </dev/tty
			#echo -n \$'\e[2A\e[s\e[K\e[m\e[1B\e[s\e[K\e[m'

	return
	# we are not loading roots profile
	chroot "${mountpoint}" /bin/su - <<-CHROOT
		# since we can't supress "stdin: is not a tty" error, the following ANSI escape sequences cursor movement erases it
		echo -n $'\e[1A\e[s\e[K\e[m'
		export PS1="(${red}${filesystem##*/}${clr})${green}\u${clr}@${green}\h${clr}:${blue}\w${clr}\$ "
		. ~/.bash_aliases
		alias
		/bin/bash --norc --noprofile </dev/tty
		#/bin/bash --norc </dev/tty
		#/bin/bash --noprofile </dev/tty
	CHROOT
	#chroot "${mountpoint}" /bin/su -c 'export debian_chroot="1234"; /bin/bash' -

	return
	# stdin: is not a tty
	chroot "${mountpoint}" /bin/su - <<-CHROOT
		export debian_chroot="1234"
		/bin/bash </dev/tty
	CHROOT

	return
	# no error
	chroot "${mountpoint}" /bin/su -

	return
	# stdin: is not a tty
	chroot "${mountpoint}" /bin/bash <<-CHROOT
		/bin/su - <<-SU
			export debian_chroot="1234"
			/bin/bash </dev/tty
		SU
	CHROOT

	return
	# groups: cannot find name for group ID 127
	# debian_chroot works
	# we don't login to the home directory	
	chroot "${mountpoint}" /bin/bash <<-CHROOT
		export debian_chroot="1234"
		echo debain_chroot=\$debian_chroot
		/bin/bash -l -i </dev/tty
	CHROOT

	return
	# no errors
	chroot "${mountpoint}" /bin/bash <<-CHROOT
		su - </dev/tty
	CHROOT

	return
	# stdin: is not a tty	
	chroot "${mountpoint}" /bin/bash <<-CHROOT
		su -
	CHROOT
	# no errors
	chroot "${mountpoint}" /bin/bash -c "su -"

	return
	# groups: cannot find name for group ID 127
	# echo exec
	chroot "${mountpoint}" <<-CHROOT
		exec </dev/tty
	CHROOT
}
__list_boot_fs_dependents () {
	local filesystem=${1:-$(get_booted_root_filesystem)}
	is_zfs_filesystem "${filesystem}" || return 1
	local zfs_pool=${filesystem%%/*}
	zfs list -t filesystem -H -o name,origin -r "${zfs_pool}" |
		sed -n "\|[[:space:]]${filesystem}@.*|s|[[:space:]]${filesystem}@.*||p"

}
__remove_fs_tree () {
	local filesystem=$1
	if is_zfs_filesystem "${filesystem}"; then
		local filesystems=$(zfs list -H -o name -t filesystem -r "${filesystem}")
	elif is_zfs_filesystem "${filesystem%/*}"; then
		if < <(< <(zfs list -H -o name -t filesystem -r "${filesystem%/*}"
		   ) grep "^${filesystem}" && echo -n $'\x255') read -d $'\x255'; then
			local filesystems=${REPLY}
		else
			return 1
		fi
	else
		return 1
	fi
	# filesystem tree should not contain the current booted root
	if < <(get_booted_root_filesystem | grep -x "${filesystems}" && echo -n $'\x255') read -d $'\x255'; then
		echo the filesystem tree\"${filesystem}\" contains the mounted root fs \"${REPLY%?}\", cancelling request for removal.
		echo
		exit 1
	fi

	# confirm that none of the leaves in the tree are volumes
	if < <(zfs list -H -o name -t volume -r "${filesystem}" | grep "" && echo -n $'\x255') read -d $'\x255'; then
		echo the filesysten tree \"${filesystem}\" contains volumes, please manually remove these.  cancelling request for removal.
		echo
		echo nested volumes
		local IFS=$'\n'
		printf "\t%s\n" ${REPLY}
		unset IFS
		echo
		exit 1
	fi

	# confirm filesystem list before proceeding
	if ${remove_fs_tree_confirm:-true}; then
		local U="\e[4m" u="\e[24m"
		local B="\e[1m" b="\e[22m"
		echo filesystems to be unmounted and removed including unlisted dependent snapshots and clones:
		<<< "${filesystems}" sed 's|^|\t|'
		echo
		echo       "are you sure you want remove these filesystems?"
		echo -n -e "[ ${U}y${u}es|${U}${B}n${u}o${b}|${U}a${u}ll|${U}c${u}ancel|${U}u${u}nmount all, then confirm ] > "
		read -s -n 1 -i n
		echo
		case "${REPLY}" in
			c|n)	echo canceling request to remove filesystem tree.
					exit 1
					;;
			a)		local destroy_fs_tree_reply='a';;
			*)		;;
		esac
	fi

	# retrieve list of mounts related to the target zfs filesystem tree
	local -A        devs mnts typs args mount_entries
	local fail err  dev  mnt  typ  arg IFS=')'
	local          fdev fmnt
	local          sdev smnt
	while read dev  mnt  typ  arg  mount_entry; do
		devs[${mnt}]=${dev}
		mnts[${mnt}]=${mnt}
		typs[${mnt}]=${typ}
		args[${mnt}]=${arg}
		mount_entries[${mnt}]=${mount_entry}
		(( ${#dev} > ${#fdev} )) && fdev=${dev//?/ }
		(( ${#mnt} > ${#fmnt} )) && fmnt=${mnt//?/ }
	done < <(
		< <(
			< <(mount) grep -x -f <(<<< "${filesystems}" sed 's/^/^/;s/$/ on .*/')
		) __sed_format_mount_entry
	)
	unset IFS

	# start unmounting the entire filesystem tree, if any unmounts fail, report, undo and exit 1
	while read mnt; do
		dev=${devs[${mnt}]}
		sdev=${fdev:${#dev}}
		smnt=${fmnt:${#mnt}}
		if ! <<< "${mount_entries[${mnt}]}" umount_fs; then
			echo umount of filesystem \"${dev}\" "${sdev}" from path \"${mnt}\" "${smnt}" failed.  restoring mounts and cancelling request for filesystem tree removal.
			echo
			while read mnt; do
				dev=${devs[${mnt}]}
				sdev=${fdev:${#dev}}
				smnt=${fmnt:${#mnt}}
				if ! <<< "${mount_entry[${mnt}]}" restore_filesystem_mount; then
					echo restoring filesystem \"${dev}\" "${sdev}" to path \"${mnt}\" "${smnt}" failed.  warning.
				else
					echo restoring filesystem \"${dev}\" "${sdev}" to path \"${mnt}\" "${smnt}" success.
				fi
			done < <(< <(< <(printf "%s\n" "${mnts[@]}") sort) grep -A10000 -x "${mnt}")
			echo
			exit 1
		else
			echo umount of filesystem \"${dev}\" "${sdev}" from path \"${mnt}\" "${smnt}" success.
		fi
	done < <(< <(printf "%s${mnts[@]+\n}" "${mnts[@]}") sort -r)

	# destroy zfs filesystems, dependant snapshots and dependant clones
	destroy_fs_tree "${filesystem}"
}
function destroy_fs_tree(){
	local filesystem=${1}
	local filesystem_type=$(zfs get type -H -o value "${filesystem}")
	if [ "${FUNCNAME}" != "${FUNCNAME[1]}" ]; then
		local IFS=$'\n'
		local REPLY="${destroy_fs_tree_reply:-n}"
		local U="\e[4m" u="\e[24m"
		local B="\e[1m" b="\e[22m"
		local message="destroy filesystem \"${filesystem}\"? "
		local question="[ ${U}y${u}es|${B}${U}n${u}o${b}|${U}a${u}ll|${U}t${u}est|${U}c${u}ancel ] > "
		local message_length=$(( ${#message} + 20))
		# destroy dependent child filesystems
		local filesystem_child
		for   filesystem_child in $(< <(< <(zfs list -H -o name -t filesystem -r "${filesystem}") grep -v -x "${filesystem}") sort -r); do
			${FUNCNAME} "${filesystem_child}"
		done
	fi
	case "${filesystem_type}" in
		filesystem)
			# destroy dependent snapshots
			local filesystem_snapshot
			for   filesystem_snapshot in $(< <(< <(zfs list -H -o name -t snapshot -r "${filesystem#/*}") grep "^${filesystem}@") sort -r); do
				${FUNCNAME} "${filesystem_snapshot}"
			done
			;;
		snapshot)
			# destroy dependent clone filesystem
			local filesystem_clone
			for   filesystem_clone in  $(< <(zfs get clones -H -o value "${filesystem}") tr , \\n); do
				${FUNCNAME} "${filesystem_clone}"
			done
			;;
		*)
			echo filesystem \"${filesystem}\" does not exist or may have already been destroyed.
			return 1
			;;
	esac
	# destroy filesystem
	message="destroy filesystem \"${filesystem}\"? "
	(( ${#message} > message_length )) && message_length=${#message}
	printf "%-$(( message_length ))s" "${message}"
	case "${REPLY}" in
		a|c|t);;*)	echo -n -e "${question}"
					read -s -n 1 -i n;;
	esac
	case "${REPLY}" in
		a|y)	echo -n destroying...
				#if true; then
				if zfs destroy "${filesystem}"; then
					echo success.
				else
					echo failed.
				fi;;
		c)		echo canceling request to remove filesystem tree.
				exit 1;;
		n|t|*)	echo skipping.;;
	esac
}
#cat /proc/*/mountinfo | sort -u
function get_mountinfo_path_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(get_mountinfo) sed -n "\?^\([^ ]\+ \)\{1\}\(${targets//|/\\|}\) ?p"
}
function get_mountinfo_dev_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(get_mountinfo) sed -n "\?^\([^ ]\+ \)\{5\}\(${targets//|/\\|}\) ?p"
}
function get_mountinfo_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(get_mountinfo) grep " \(${targets//|/\\|}\) "
}
function get_mountinfo(){
	< <(< <(< <(cat /proc/*/mountinfo 2> /dev/null) sort -u) sed 's/^\([^ ]\+ \)\{3\}//') sort -u
}
function is_mounted(){
	local target=${1}
	< <(mount) grep -q "\(^${target} on \| on ${target} type \)"
}
function get_mount_sub_entries(){
	local targets=( "$@" )
	local targets=( "${targets[@]%/}" )
	local targets=( "${targets[@]/%/$'/.* type '}" )
	local targets=$(IFS="|";echo "${targets[*]}";)
	< <(mount) sed -n "\?\( on \(${targets//|/\\|}\)\)?p"
}
function get_mount_dev_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(mount) sed -n "\?\(^\(\${targets//|/\\|}\) on \)?p"
}
function get_mount_path_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(mount) sed -n "\?\( on \(${targets//|/\\|}\) type \)?p"
}
function get_mount_entries(){
	local targets=$(IFS="|";echo "$*";)
	< <(mount) sed -n "\?\(^\(\${targets//|/\\|}\) on \| on \(${targets//|/\\|}\) type \)?p"
}
function get_mount_entry(){
	local target=${1}
	< <(mount) sed -n "\?\(^${target} on \| on ${target} type \)?p"
}
function umount_subtree(){
	local mount_entries=$(get_mount_sub_entries "$@")
	local IFS=')'  paths fill err
	local      dev path typ opt mount_entry
	while read dev path typ opt mount_entry; do
		paths[${#paths[*]}]=${path}
		(( ${#path} > ${#fill} )) && fill=${path//?/.}
	done < <(<<< "${mount_entries}" __sed_format_mount_entry)

	# reverse sort paths so that leafs are dis-mounted first
	local IFS=$'\n'
	paths=( $(< <(printf "%s\n" "${paths[@]}") sort -r) )

	# iterate threw and dis-mount paths
	for path in "${paths[@]}"; do
		umount "${path}" &> /dev/null
		echo $? "${path}" "${fill:${#path}}"
	done
}
function mount_fs(){
	local filesystem=${1}
	is_zfs_filesystem "${filesystem}" || return 1
	local mountpoint=$(zfs get mountpoint -H -o value "${filesystem}")
	case "${mountpoint}" in
		legacy)	mount "${filesystem}";;
		*)		zfs mount "${filesystem}";;
	esac
}
function mount_fs_tree(){
	local filesystem=${1}
	is_zfs_filesystem "${filesystem}" || return 1
	local IFS=$'\n'
	local devs=( $(< <(zfs list -H -o name -t filesystem -r "${filesystem}") ) )
	unset IFS
	local sdev fdev dev mnt typ arg mount_entry
	for dev in "${devs[@]}"; do (( ${#dev} > ${#fdev} )) && fdev=${dev//?/ }; done
	for dev in "${devs[@]}"; do
		sdev=${fdev:${#dev}}
		if is_mounted "${dev}" || mount_fs "${dev}"; then
			< <(< <(get_mount_entry "${dev}") __sed_format_mount_entry) IFS=')' read dev mnt typ arg mount_entry
			echo filesystem \"${dev}\" "${sdev}" is mounted to path \"${mnt}\".
		else
			echo filesystem \"${dev}\" "${sdev}" will not mount or is not a valid filesystem.
		fi
	done
}
function umount_fs(){
	# this function expects a single line of input as formated by the mount command
	local mnt dev typ arg
	local tries=${1:-3}
	local IFS=')'
	< <(__sed_format_mount_entry) read dev mnt typ arg mount_entry
	while (( tries-- )); do
		umount "${mnt}"
		if is_mounted "${mnt}"; then
			# try again
			continue
		elif is_mounted "${dev}"; then
			let tries++
			< <(< <(get_mount_entry "${dev}") __sed_format_mount_entry) read dev mnt typ arg mount_entry
		else
			# success
			return 0
		fi
	done
	return 1
}
function restore_filesystem_mount(){
	# this function expects a single line of input as formated by the mount command
	local mnt dev typ arg
	local tries=${1:-3}
	local IFS=')'
	< <(__sed_format_mount_entry) read dev mnt typ arg mount_entry
	while true; do
		if < <(mount) grep -q -x "${mount_entry}"; then
			return 0
		elif ! (( tries-- )); then
			return 1
		else
			case "${typ}" in
				zfs)
					local mountpoint=$(zfs get mountpoint -H -o value "${dev}")
					if [ "${mountpoint}" == "${mnt}" ]; then
						zfs mount "${dev}"
					elif [ "${mountpoint}" == "legacy" ]; then
						mount -t "${typ}" -o ${arg} "${dev}" "${mnt}"
					else
						echo mount path \"${mnt}\" for zfs filesystem \"${dev}\" does not match mountpoint \"${mountpoint}\".  warning, not attempting mount. 1>&2
						break
					fi
					;;	
				*)
					mount -t "${typ}" -o ${arg} "${dev}" "${mnt}"
					;;
			esac
		fi
	done
}
__sed_format_mount_entry() {
	# this function converts a mount entry string
	#	[DEVICE] on [PATH] type [filesystem type] ([mount args,comma delimited])
	# to a list of entries delimited by a closing parenthesis
	#	[DEVICE])[PATH])[filesystem type])[mount args,comma delimited])[mount entry string]
	# tp parse set local IFS=')' and use the read built-in
	sed 'h;s/ type \([^ ]\+\) (/)\1)/;s/ on /)/;G;s/\n//;s/)$/\\)/'
}

__remove_boot_fs () {
	local filesystem=$1
	is_zfs_filesystem "${filesystem}" || return 1
	local zfs_stor="${zfs_grub_base}/${filesystem}"
	# filesystem should not be current booted root
	if get_booted_root_filesystem | grep -q -x "${filesystem}"; then
		echo the boot filesystem \"${filesystem}\" is the mounted root, cancelling request for removal.
		echo
		exit 1
	fi

	# filesystem should not have dependent clones
	if < <(do_dependent_zfs_filesystems_list "${filesystem}") read; then
		echo the boot filesystem \"${filesystem}\" has dependent clone filesystems, cancelling request for removal.
		echo
		echo Dependent Clone Filesystems
		local IFS=$'\n'
		printf "\t%s\n" ${REPLY}
		unset IFS
		echo
		exit 1
	fi

	# filesystem should not have any nested filesystems or volumes
	if < <(zfs list -t volume,filesystem -H -o name  -r -d 1 "${filesystem}" | grep -v -x "${filesystem}" && echo -n $'\x255') read -d $'\x255'; then
		echo the boot filesystem \"${filesystem}\" has nested filesystems or volumes, cancelling request for removal.
		echo
		echo Nested Filesystems or Volumes
		echo "${REPLY}"
		exit 1
	fi

	# the grub stor should exist and not have any sub-folders
	if [ -e "${zfs_stor}" ] && ! [ -d "${zfs_stor}" ]; then
			echo the filesystem \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" exists but is not a directory, cancelling request for removal.
			exit 1
	elif [ -d "${zfs_stor}" ] && < <(find "${zfs_stor}" -maxdepth 1 -mindepth 1 -type d && echo -n $'\x255') read -d $'\x255'; then
			echo the filesystem \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" exists but has sub-directories, cancelling request for removal.
			exit 1
	elif ! [ -e "${zfs_stor}" ]; then
			echo the filesystem \"${filesystem%/}\" grub stor \"${zfs_grub_base}/${filesystem%/}\" does not exists, cancelling request for removal.
			exit 1
	fi

	# destroy filesystem and related snapshots automatically
	if ! zfs destroy -r "${filesystem}"; then
		echo the filesystem \"${filesystem%/}\" could not be recursivelly destroyed, cancelling request for removal.
		exit 1
	fi

	# remove grub stor
	rm -rvf -- "${zfs_stor}"
}
__clone_boot_zfs_filesystem () {
	local filesystem alt_zfs_name alt_version alt_zfs_base
	while [[ "${1//[^[:alpha:]-]/_}" != +(--|) ]]; do
			case "${1}" in
				--name)		shift
							alt_zfs_name=$1;;
				--version)	shift
							alt_version=0000${1//[^[:digit:]]/}
							alt_version=${alt_version: -4};;
				--zfs-base)	shift
							alt_zfs_base=$1
							vet_zfs_base "${alt_zfs_base}";;
				*)			is_zfs_filesystem "$1" && filesystem=$1;;
			esac
			shift
	done
	local filesystem=${filesystem:-$(get_booted_root_filesystem)}
	local zfs_stor="${zfs_grub_base}/${filesystem%/}"
	local OS=$(get_OS)
	
	# set base zfs filesystem
	(( ${alt_zfs_base:+1} )) && local zfs_base=${alt_zfs_base} || local zfs_base=$(dirname "${filesystem%/}")

	# verify that the filesystem to be cloned has a zfs grub stor with files in it
	# we are assuming that if it does that amongst those files are kernels and initrds
	# we also assume that retained versions (files that end in *.v[:digit:][:digit:]) are not esential
	if ! __verify_zfs_grub_stor "${filesystem%/}"; then
		echo cancelling request for clone.
		exit 1
	elif [ -d "${zfs_grub_base}/${filesystem%/}" ] &&
		 ! find "${zfs_grub_base}/${filesystem%/}" -maxdepth 1 -type f | sed -n 2p | read; then
		echo selected zfs filesystem \"${filesystem%/}\" to clone has less than 2 files
		echo in it\'s zfs grub stor \"${zfs_grub_base}/${filesystem%/}\", cancelling request for clone.
		exit 1
	elif [ -d "${zfs_grub_base}/${filesystem%/}" ] &&
		 find "${zfs_grub_base}/${filesystem%/}" -maxdepth 1 -mindepth 1 -type d | read; then
		echo selected zfs filesystem \"${filesystem%/}\" to clone has directories
		echo in it\'s zfs grub stor \"${zfs_grub_base}/${filesystem%/}\", cancelling request for clone.
		exit 1
	fi

	# if a new name is supplied then date or version components of the filesystem name do not need to be verified
    # if the name exists then the version number if not supplied will be automatically calculated
	if (( ${alt_zfs_name:+1} )); then
		local zfs_name=${alt_zfs_name}
	# if only a version number is supplied verify that date and version components are not malformed and
	# that the supplied version number is equal to or greater than what would have been automatically calculated
	elif (( ${alt_version:+1} )); then
		if is_date_in_filesystem_name_malformed "${filesystem}"; then
			echo filesystem name \"${filesystem}\" does not have the expected date component or it is malformed.
			echo filesystem name is expected to be in the format [name]-[version]-[date +%Y.%m.%d].
			echo we will not be able to extrapolate the appropriate name component for the clone.
			echo either don\'t specify a version or specify a new name, cancelling request for clone.
			exit 1
		elif is_version_in_filesystem_name_malformed "${filesystem}"; then
			echo filesystem name \"${zfs_name}\" does not have the expected version component or it is malfomed.
			echo filesystem name is expected to be in the format [name]-[version]-[date +%Y.%m.%d].
			echo we will not be able to extrapolate the appropriate name component for the clone.
			echo either don\'t specify a version or specify a new name, cancelling request for clone.
			exit 1
		else
			local zfs_name=$(get_name_in_filesystem_name "${filesystem}")
			
		fi
	# if neither a name nor a version are supplied then check that date and version are not malformed
	# if date or version are malformed then assume the filesystem name is the base name for the clone
	else
		if is_date_in_filesystem_name_malformed "${filesystem}"; then
			local zfs_name=$(basename "${filesystem%/}")
		elif is_version_in_filesystem_name_malformed "${filesystem}"; then
			local zfs_name=$(basename "${filesystem%/}")
		else
			local zfs_name=$(get_name_in_filesystem_name "${filesystem}")
		fi
	fi
	# calculate version
	local version=$(get_next_version_for_filesystem_name "${zfs_base}" "${zfs_name}")
	# if alternate version is selected verify that it is in range
	if (( ${alt_version:+1} )); then
		if (( alt_version >= version )); then
			version=${alt_version}
		else
			echo the selected alternate version number \"${alt_version}\" is not within range.
			echo the version must be greater than to equal to the \"${version}\", cancelling request for clone.
			exit 1
		fi
	fi
	# calculate date, prep vars
	local zfs_date=$(get_date_for_clone_filesystem_name)
	local zfs_snap="${filesystem}@${zfs_name}-${version}-${zfs_date}"
	local zfs_clone="${zfs_base}/${zfs_name}-${version}-${zfs_date}"
	local zfs_grub_seed="${zfs_grub_base}/${filesystem}"
	local zfs_grub_stor="${zfs_grub_base}/${zfs_clone}"

	# make zfs_grub_stor and copy files from zfs_grub_seed
	# zfs_grub_stor should either not exists or be empty if not we should exit with error
	if [ -e "${zfs_grub_stor}" ] && ! [ -d "${zfs_grub_stor}" ]; then
		echo zfs_grub_stor \"${zfs_grub_stor}\" for clone filesystem \"${zfs_clone}\" exists but is not a directory.
		echo please rectify this before proceeding, try again when ready, cancelling request for clone.
		exit 1
	elif [ -d "${zfs_grub_stor}" ] && ! find "${zfs_grub_stor}" -maxdepth 0 -empty | read; then
		echo zfs_grub_stor \"${zfs_grub_stor}\" for clone filesystem \"${zfs_clone}\" exists but is not empty.
		echo please rectify this before proceeding, try again when ready, cancelling request for clone.
		exit 1
	else
		echo creating zfs_grub_stor \"${zfs_grub_stor}\"
		mkdir -p "${zfs_grub_stor}"
		echo populating zfs_grub_stor \"${zfs_grub_stor}\"
		find "${zfs_grub_seed}" -mindepth 1 -not -iname '*.v??' -exec cp -f '{}' "${zfs_grub_stor}" ';'
	fi

	# snapshot filesystem, if this snapshot already exists then skip this step
	if zfs list -t snapshot -H -o name "${zfs_snap}" &>/dev/null; then
		echo snapshot \"${zfs_snap}\" already exists, this must be a repeat try, moving to clone.
		echo if you need a fresh snapshot then please remove this one using the command \"zfs destroy [snapshot]\"
	else
		echo taking zfs snapshot \"${zfs_snap}\"
		zfs snapshot "${zfs_snap}"
	fi
	# clone snapshot
	if zfs list -t filesystem -H -o name "${zfs_clone}" &>/dev/null; then
		echo clone filesystem \"${zfs_clone}\" already exists, this must be a repeat try, moving to grub_stor.
		echo if you need a fresh clone then please remove this one using the command \"zfs destroy [filesystem]\"
	else
		echo generating zfs clone filesystem \"${zfs_clone}\"
		zfs clone "${zfs_snap}" "${zfs_clone}"
	fi

	echo
	echo ...... zfs_name :: ${zfs_name}
	echo ....... version :: ${version}
	echo ...... zfs_date :: ${zfs_date}
	echo
	echo ...... zfs_base :: ${zfs_base}
	echo .... filesystem :: ${filesystem}
	echo ...... zfs_snap :: ${zfs_snap}
	echo ..... zfs_clone :: ${zfs_clone}
	echo
	echo . zfs_grub_base :: ${zfs_grub_base}
	echo . zfs_grub_seed :: ${zfs_grub_seed}
	echo . zfs_grub_stor :: ${zfs_grub_stor}
	echo

	# run grub update
	update-grub
}
function is_version_in_filesystem_name_malformed(){
	local filesystem=$1
	(( ${filesystem:+1} )) || return 1
	local version=$(get_version_in_filesystem_name "${filesystem}")
	! [[ "${version//[^[:digit:]]/}" =~ ^[[:digit:]]{4}$ ]]
}
function is_date_in_filesystem_name_malformed(){
	local filesystem=$1
	(( ${filesystem:+1} )) || return 1
	local date=$(get_date_in_filesystem_name "${filesystem}")
	! date --date="${date//./\/}" &>/dev/null
}
function get_version_in_filesystem_name(){
	local filesystem=$1
	(( ${filesystem:+1} )) || return 1
	echo "${filesystem: -15:4}"
}
function get_date_in_filesystem_name(){
	local filesystem=$1
	(( ${filesystem:+1} )) || return 1
	echo "${filesystem: -10}"
}
function get_name_in_filesystem_name(){
	local filesystem=$1
	(( ${filesystem:+1} )) || return 1
	basename "${filesystem::-16}"
}
function get_date_for_clone_filesystem_name(){
	date +%Y.%m.%d
}
function get_next_version_for_filesystem_name(){
	local filesystem=$1
	local name=$2
	local version
	(( ${filesystem:+1} )) || return 1
	< <(zfs list -t filesystem -H -o name -d 1 -r "${filesystem%/}" |
		grep "^${filesystem%/}/${name}-" |
		while read filesystem; do
			is_version_in_filesystem_name_malformed "${filesystem}" ||
			get_version_in_filesystem_name "${filesystem}"
		done |
		sort -r) read version
	version=000$(( ${version:- -1} + 1 ))
	echo ${version: -4}
}
function get_cache(){
	local func_to_cache=$1
	if declare -F "${func_to_cache}" &>/dev/null ; then
		shift
	else
		# testing to see if the calling function has a variant defined by DELIM and TAILS
		# DELIM current equals "_" and ""
		# as an example, if the calling function is myFunc() then the following variants would be sought 
		#   myFunccache
		#   myFuncCache
		#   myFuncCACHE
		#   myFunc_cache
		#   myFunc_Cache
		#   myFunc_CACHE
		local tail tails delim
		while true; do
			while read tails; do
				while read delim; do
					for tail in ${tails} ${tails^} ${tails^^}; do
						# test for parent funcname variants by delim and tail
						[[ "${FUNCNAME[1]}" =~ (.*)("${delim}${tail}"$) ]] &&
							declare -F "${BASH_REMATCH[1]}" &>/dev/null &&
							func_to_cache="${BASH_REMATCH[1]}" &&
							break 4
					done
				done <<-DELIM
					_

				DELIM
			done <<-TAILS
				cache
			TAILS
			# if all function name tests fail, fail this
			return 1
		done
	fi
	# time till cache is removed in minutes
	local cache_refresh_gate=${1:-2}

	# define cache file
	local cache="/tmp/.$(basename "$0")_${func_to_cache}_${PPID}"

	# check for get_cache_clear flag
	if ${get_cache_reset:-false}; then
		rm -f "${cache}"
	fi

	# if cache exists return cache
	if [ -e "${cache}" ] && [ -s "${cache}" ]; then
		cat "${cache}"
	# else set cache clear job, run and tee to cache 
	else
		cat <<-AT | at -q c now + ${cache_refresh_gate} minutes 2>/dev/null
			/bin/rm -f "${cache}"
		AT
		< <(${func_to_cache} 2>/dev/null) tee "${cache}"
	fi
}


# function readfd: file descriptor is flushed to variable REPLY[${fd}] and printed to stdout; (${fd} is the supplied file descriptor)
#                   multiple fd's can be requested, only the last fd reports false if empty (not the same as closed)
function readfd(){
	local _name _fd IFS=$''
	(( $# )) || set -- 3
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		if [ -e "/proc/${BASHPID}/fd/${_fd}" ]; then
			echo    -n $'\255'  >&${_fd}
			# the "-r" argument in the following read command preserves escaped characters
			read -r -d $'\255' -u ${_fd} REPLY[${_fd}]
			case "${FUNCNAME[1]}" in
				flushfd)	;;
				*)		printf %s "${REPLY[${_fd}]}";;
			esac
		else
			unset REPLY[${_fd}]
		fi
	done
	(( ${#REPLY[${_fd}]} ))
}
# function flushfd: is quiet readfd.
#                   file descriptor is flushed to variable REPLY[${fd}] but not printed; (${fd} is the supplied file descriptor)
#                   multiple fd's can be requested, only the last fd reports false if empty (not the same as closed)
function flushfd(){
	readfd "$@"
}
# function teefd: first arg is the file desciptor to print and clone
#                 remaining args are the file desciptors to create/clone
#                 if all or all but the first arg are un-set then teefd prints the fd and reclones into itself
function teefd(){
	local _fd _name _sfd=${1:-3}
	case "${FUNCNAME[1]}" in
		clonefd)	flushfd ${_sfd};;	
		*)		readfd ${_sfd};;
	esac
	shift
	(( $# )) || set -- ${_sfd}
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		[ -e "/proc/${BASHPID}/fd/${_fd}" ] || eval "exec ${_fd}<><(:)"
		printf %s "${REPLY[${_sfd}]}" >&${_fd}
	done
	(( ${#REPLY[${_sfd}]} ))
}
# function clonefd: is teefd quiet; arguments work the same
function clonefd(){
	teefd "$@"
}
function mkfd(){
	local _fd _name
	(( $# )) || set -- 3
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		[ -e "/proc/${BASHPID}/fd/${_fd}" ] || eval "exec ${_fd}<><(:)"
	done
}
function clearfd(){
	local _fd _name
	(( $# )) || set -- 3
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		eval "exec ${_fd}<><(:)"
	done
}
function closefd(){
	local _fd _name
	(( $# )) || set -- 3
	for _fd in $*; do
		_name=${_fd##[^[:alpha:]_]*}
		_fd=${!_name:=${_fd}}
		eval "exec ${_fd}<&-"
	done
}
function randomfd(){
	local _fd
	for _fd in $*; do
		local _loop='3'
		while (( _loop-- )); do
			eval "exec {${_fd}}<><(:)" 2>/dev/null && break
		done
		unset REPLY[${_fd}] &> /dev/null
	done
}

# GLOBAL VARS
zfs_grub_base='/boot/zfs'
grub_vmlinuz=
__grub_dir='/boot/grub'
main "$@"
