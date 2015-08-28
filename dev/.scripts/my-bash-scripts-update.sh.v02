#!/bin/bash

function main(){
	# https://raw.githubusercontent.com/michaelsalisbury/clixs/master/dev/.scripts/my-bash-scripts-update.sh
	GIT="https://raw.githubusercontent.com"
	USER="michaelsalisbury"
	REPO="clixs"
	BRANCH="master"

	# my-zfs-utils
	SUB="dev/.scripts"
	DST="${HOME}/.bash_scripts.d"
	get "my-bash-scripts-update.sh" +x
	get "60_zfs"                    +x
	get "my-zfs-utils.sh"           +x

	SUB="dev/.scripts/bash_completion"
	DST="${HOME}"
	get ".bash_completion"
	get ".bash_completion.d/grub-mod"
	get ".bash_completion.d/vboxmanage_completion.bash"

	SUB="dev/.scripts/bash_aliases"
	DST="${HOME}"
	add ".bash_aliases"

}
function get(){
	local TRG=$1
	local MOD=$2
	local SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	mkdir -p "${DST}"
	wget  -O "${DST}/${TRG}" "${SRC}/${TRG}"
	(( ${MOD:+1} )) &&
	chmod ${MOD} "${DST}/${TRG}"
}
function add(){
	# add/append or update/replace
	local TRG=$1
	local SST=${2:-####} # START/STOP TAG/LINE comment character
	local SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	local LBL="${SST} [${SRC}/${TRG}] ${SST}"
	# load both the target content and the destination file
	local TMP=$(<"${DST}/${TRG}")
	local ADD=$(wget -q -O- "${SRC}/${TRG}")
	# find the BEGIN and END lines for the content to be replaced
	local       BEGIN END
	read -d $'' BEGIN END < <(< <(<<< "${TMP}" grep -n -F -x "${LBL}") cut -f1 -d:)
	# if BEGIN was found then update/replace otherwise add/append
	if (( ${BEGIN:+1} )); then
		(	<<< "${TMP}" head -${BEGIN}
			<<< "${ADD}" cat
			<<< "${TMP}" tail -n +${END}
		) > "${DST}/${TRG}"
	else
		mkdir -p "${DST}"
		(	<<< "${LBL}" cat
			<<< "${ADD}" cat
			<<< "${LBL}" cat
		) >> "${DST}/${TRG}"
	fi
}

which wget &>/dev/null || { echo Please install wget. Exiting.; exit 1; }
main "$@"

