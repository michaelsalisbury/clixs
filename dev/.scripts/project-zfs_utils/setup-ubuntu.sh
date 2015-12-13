#!/bin/bash

false && {
	apt-get -y install vim
	apt-get -y install curl software-properties-common debconf-utils # add-apt-repository
	apt-get -y upgrade
	apt-get -o Dpkg::Options::="--force-confnew" --force-yes -fuy dist-upgrade
	#DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade
	# grub2 apt-get auto answer
	echo "grub-pc grub-pc/install_devices_empty   boolean true" | debconf-set-selections
 	# this installs grub2
	DEBIAN_FRONTEND=text apt-get -y install linux-image-generic linux-headers-generic

}
#add-apt-repository universe multiverse restricted

function main(){
	#setup_sources_list
	#apt-get update 2>&1 |
	#	awk '/NO_PUBKEY/{print $NF}' |
	#	xargs apt-key adv --keyserver keyserver.ubuntu.com --recv-keys
	#apt-get update
	apt-get -y install linux-image-generic linux-headers-generic
	apt-get -y install dkms build-essential

	apt-get -y install spl spl-dkms zfs-dkms zfsutils
	apt-get -y install zfs-initramfs
	
	
	




}
function setup_sources_list(){
	local codename=$(lsb_release -cs)

	# OpenZFS ppa
	if curl --output /dev/null --silent --head --fail "http://ppa.launchpad.net/zfs-native/stable/ubuntu/dists/${codename}/main"; then
		echo "deb http://ppa.launchpad.net/zfs-native/stable/ubuntu ${codename} main" > /etc/apt/sources.list.d/zfs-native-stable.list
		echo "deb-src http://ppa.launchpad.net/zfs-native/stable/ubuntu ${codename} main" >> /etc/apt/sources.list.d/zfs-native-stable.list
	fi

	# main repos
	mk_version "/etc/apt/sources.list"
	echo
	local      distro URL codename repos
	while read distro URL codename repos; do
		[ "${distro}" == "deb" ] && [[ "${URL:0:4}" == +(http|HTTP) ]] && [ "${codename}" == "$(lsb_release -cs)" ] && break
		false
	done < <(sed -n '/^[[:space:]]*#/d;p' /etc/apt/sources.list)
	(( $? )) && return 1
	(
		for   repo in "" -updates -backports -security; do
			sub=""
			for verse in main universe multiverse restricted; do
				if curl --output /dev/null --silent --head --fail "${URL}/dists/${codename}${repo}/${verse}"; then
					sub+=" ${verse}"
				fi
			done
			if (( ${#sub} )); then
				echo "${distro} [arch=amd64,i386] ${URL} ${codename}${repo}${sub}"
				echo "${distro}-src [arch=amd64,i386] ${URL} ${codename}${repo}${sub}"
			fi
		done

		if curl --output /dev/null --silent --head --fail "http://archive.canonical.com/ubuntu/dists/${codename}/partner"; then
			echo
			echo "${distro} [arch=amd64,i386] http://archive.canonical.com/ubuntu ${codename} partner"
			echo "${distro}-src [arch=amd64,i386] http://archive.canonical.com/ubuntu ${codename} partner"
		fi

		if curl --output /dev/null --silent --head --fail "http://extras.ubuntu.com/ubuntu/dists/${codename}/main"; then
			echo
			echo "${distro} [arch=amd64,i386] http://extras.ubuntu.com/ubuntu ${codename} main"
			echo "${distro}-src [arch=amd64,i386] http://extras.ubuntu.com/ubuntu ${codename} main"
		fi

	) | tee /etc/apt/sources.list
	echo
	mk_version "/etc/apt/sources.list"
}
function mk_version(){
	local _target=$1
	local _versions_base=${2:-$1}
	local _versions_to_keep='3' # this is only effective when the archive directory exists
	local _archive=${_target%${_target##*/}}
	      _archive="${_archive}${_archive:+/}archive"
	local _archives_base="${_archive}/${_versions_base##*/}"
	get_version "$@" && return
	randomfd fd_AWK && cat <<-AWK >& ${fd_AWK}
		END{
			LEN=length(\$NF)
			\$NF="0000"(\$NF+1)
			print (LEN?substr(\$NF,length(\$NF)-LEN+1,LEN):0)
		}
	AWK
	local _version=$(ls -1 "${_versions_base}".v* 2>/dev/null)
	local _version=$(awk -Fv -f <(readfd fd_AWK) <<< "${_version}")
	if [[ "${_version}" =~ ^9+$ ]]; then
		local IFS=$'\n'
		for _targets in `ls -1 "${_versions_base}".v* "${_archives_base}".v* 2>/dev/null`; do
			mv -f "${_targets}" "${_targets%v*}v0${_targets##*v}" &
		done
		unset IFS
		wait
		_version="0${_version}"
	fi
	cat "${_target}" > "${_versions_base}.v${_version}"
	[ -x "${_target}" ] && chmod +x "${_versions_base}.v${_version}"
	# if archive folder exist move all but the 3 most current
	# we perform a copy and delete instead of move because it's harder to track with incrond
	echo "${_archive}"
	if [ -d "${_archive}" ]; then
		cp -n -- $(head -n -${_versions_to_keep} <(ls -1 "${_versions_base}".v*)) "${_archive}/". 2>/dev/null
		rm -f -- $(head -n -${_versions_to_keep} <(ls -1 "${_versions_base}".v*))
	fi
	get_version "$@"
}
function get_version(){
	local _target=$1
	local _versions_base=${2:-$1}
	local _archive="${_target%/*}/archive"
	local _archives_base="${_archive}/${_versions_base##*/}"
	local    fd_sha1sum fd_vlist fd_alist
	randomfd fd_sha1sum fd_vlist fd_alist
	# process archives if they exist
	[ -d "${_archive}" ]                                       &&
	sha1sum "${_archives_base}".v* 2>/dev/null  >& ${fd_alist} &&
	< <(readfd fd_alist) sed "s|$| ${_target}|" >& ${fd_alist} &
	# process versions
	sha1sum "${_versions_base}".v* 2>/dev/null  >& ${fd_vlist} &&
	< <(readfd fd_vlist) sed "s|$| ${_target}|" >& ${fd_vlist} &
	# generate target sha1sum
	cat     "${_target}"                        >& ${fd_sha1sum} &&
	< <(readfd fd_sha1sum) sha1sum              >& ${fd_sha1sum} &&
	< <(readfd fd_sha1sum) awk '{print $1}'     >& ${fd_sha1sum} &
 	wait
	# return sha1sum and versioned file that matches target
	grep -h -F -f <(readfd fd_sha1sum) <(readfd fd_vlist) <(readfd fd_alist)
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
main "$@"
exit $?
