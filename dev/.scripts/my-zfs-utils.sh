#!/bin/bash


function main(){
	if declare -F "do_${1}" &>/dev/null; then
		local cmd=$1
		shift
		do_$cmd "$@"
	else
		shift
		do_help "$@"
	fi
}
function do_help(){
	declare -F | sed -n 's/^declare -f do_//p'
}
function do_dev(){
	get_booted_root_filesystem
}
function do_boot_list(){
	__grub_list_menuentries_raw | __sed_escape_string 1
}
function do_grub_set_default(){
	grub-set-default "${1}"
}
function do_grub_reboot(){
	grub-reboot "${1}"
}
function do_update_grub(){
	update-grub
}
function do_chroot_zfs_filesystem(){
	:
}
function do_update_zfs_initrd(){
	:
	local filesystem=${1:-$(get_booted_root_filesystem)}
	if ! is_zfs_filesystem "${filesystem}"; then
		cat <<-ERROR 1>&2
			${filesystem}\" is not a zfs filesystem.
		ERROR
	elif ! [ -d "${zfs_grub_root}/${filesystem}" ]; then
		cat <<-ERROR 1>&2
			The zfs grub kernel/initrd dir does not exists; "${zfs_grub_root}/${filesystem}".
			If the filesystem "${filesystem}" is bootable please use the following commands to update grub.
			# $0 add_zfs_filesystem_to_grub ${filesystem}
			# $0 update_grub
		ERROR
	fi
	


}
function do_clone_zfs_filesystem(){
	:
}
function do_add_zfs_filesystem_to_grub(){
	:
}
function get_booted_root_filesystem(){
	mount | awk '$3=="/"{print $1}'
}
function is_zfs_filesystem(){
	local filesystem=${1:-$(get_booted_root_filesystem)}
	zfs list -H -o name "${filesystem}" &>/dev/null
}
function get_(){
	:

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
# GLOBAL VARS
zfs_grub_root='/boot/zfs'
main "$@"
