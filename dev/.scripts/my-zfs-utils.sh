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
__sed_escape_string () {
	local sed_special_chars=$' >'
	sed 's/\(['"${sed_special_chars}"']\)/\\\1/g'
}
function do_boot_list(){
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
function do_grub_set_default(){
	grub-set-default "${1}"
}
function do_grub_reboot(){
	grub-reboot "${1}"
}
function get_booted_root_filesystem(){
	mount |
	awk '$3=="/"{print "zfs list -H -o name "$1}' |
	bash 2>/dev/null
}



main "$@"
