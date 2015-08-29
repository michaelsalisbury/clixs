#!/usr/local/bin/bash


function auth_daemon_winbindd(){
	local action=$1
	shift
	local ID=$*
	case "${action}" in
		clear)	winbindd_header      > "${pam_auth_config}";;
		#clear)	winbindd_header;;
		add)	winbindd_auth ${ID} >> "${pam_auth_config}";;
		#add)	winbindd_auth ${ID};;
		*)	echo "$@";;
	esac
}
function auth_daemon_sssd(){
	:
	echo "$@"
}
function winbindd_header(){
	cat <<-HEADER
		#
		# PAM configuration for the "pwauth" service
		#
	HEADER
}
function winbindd_auth(){
	local ID=$*
	wbinfo -n "${ID}" &>/dev/null || return 1
	local SID=$(wbinfo -n "${ID}" 2>/dev/null)
	local NAME=$(wbinfo -s ${SID%% *})
	cat <<-AUTH
		# ${NAME}
		auth sufficient /usr/local/lib/pam_winbind.so silent try_first_pass krb5_auth krb5_ccache_type=FILE require_membership_of=${SID%% *}
	AUTH
}
function main(){
	local      auth_daemon action args
	while read auth_daemon action args; do
		eval declare -f "${auth_daemon/#/auth_daemon_}" &>/dev/null || continue
		eval "${auth_daemon/#/auth_daemon_}" \"\${action}\" \"\${args}\"
		#echo ${auth_daemon} ${action} ${args}
	done
}
[ $# -gt 0 ] &&
	pam_auth_config=$1 ||
	pam_auth_config="/etc/pam.d/pwauth.owncloud"
main "$@"
