#!/bin/bash

which wget &>/dev/null || { echo Please install wget. Exiting.; exit 1; }

# https://raw.githubusercontent.com/michaelsalisbury/clixs/master/dev/.scripts/my-bash-scripts-update.sh
GIT="https://raw.githubusercontent.com"
USER="michaelsalisbury"
REPO="clixs"
BRANCH="master"

function get(){
	TRG=$1
	SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	mkdir -p "${DST}"
	wget  -O "${DST}/${TRG}" "${SRC}/${TRG}"
}

# my-zfs-utils
SUB="dev/.scripts"
DST="/root/.bash_scripts.d"
get "my-bash-scripts-update.sh"



