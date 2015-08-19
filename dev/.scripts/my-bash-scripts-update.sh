#!/bin/bash

which wget &>/dev/null || { echo Please install wget. Exiting.; exit 1; }

# https://raw.githubusercontent.com/michaelsalisbury/clixs/master/dev/.scripts/my-bash-scripts-update.sh
GIT="https://raw.githubusercontent.com"
USER="michaelsalisbury"
REPO="clixs"
BRANCH="master"

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
	# add, append, update or replace
	local TRG=$1
	local SST=${2:-#} # START/STOP TAG/LINE comment character
	local SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	local TMP=$(cat "${DST}/${TRG}")
	local TRG=$(wget -O- "${SRC}/${TRG}")
	mkdir -p "${DST}"
	if grep -q "^${SST} ${SRC}/${TRG}$" <<< "${TMP}"; then
		:
	else
		:
	fi
}

# my-zfs-utils
SUB="dev/.scripts"
DST="/root/.bash_scripts.d"
get "my-bash-scripts-update.sh" +x
get "60_zfs"                    +x
get "my-zfs-utils.sh"           +x

DST="/root"
get "bash_completion/.bash_completion"

DST="/root/.bash_completion.d"
get "bash_completion/.bash_completion.d/grub-mod"
get "bash_completion/.bash_completion.d/vboxmanage_completion.bash"

