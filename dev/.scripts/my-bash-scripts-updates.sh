#!/bin/bash

which wget &>/dev/null || { echo Please install wget. Exiting.; exit 1; }

# https://raw.githubusercontent.com/michaelsalisbury/clixs/master/dev/.scripts/my-bash-scripts-updates.sh
GIT="https://raw.githubusercontent.com"
USER="michaelsalisbury"
REPO="clixs"
BRANCH="master"

function get(){
	SRC="${GIT}/${USER}/${REPO}/${BRANCH}/${SUB}"
	mkdir -p "${DST}"
	wget  -O "${DST}/${TRG}" "${SRC}/${TRG}"
}

# my-zfs-utils
SUB="dev/.scripts"
TRG="my-bash-scripts-updates.sh"
DST="/root/.bash_scripts.d"
get

