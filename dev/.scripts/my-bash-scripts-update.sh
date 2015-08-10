#!/bin/bash

which wget &>/dev/null || { echo Please install wget. Exiting.; exit 1; }

# https://raw.githubusercontent.com/michaelsalisbury/clixs/master/dev/.scripts/my-bash-scripts-update.sh
GIT="https://raw.githubusercontent.com"
USER="michaelsalisbury"
REPO="clixs"
BRANCH="master"

function get(){
	TRG=$1
	MOD=$2
	SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	mkdir -p "${DST}"
	wget  -O "${DST}/${TRG}" "${SRC}/${TRG}"
	(( ${MOD:+1} )) &&
	chmod +x "${DST}/${TRG}"
}

# my-zfs-utils
SUB="dev/.scripts"
DST="/root/.bash_scripts.d"
get "my-bash-scripts-update.sh" +x
get "60_zfs"                    +x
get "my-zfs-utils.sh"           +x

