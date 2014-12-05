#!/bin/bash

function main(){
	local ROOT='/usr/local/share'
	local REPO='clixs'
	local GIT='michaelsalisbury'
	# verify package instalation
	if ! dpkg -l ${REPO} &>/dev/null; then
		echo Looks like the \"${REPO}\" package was not installed yet. 1>&2
		echo Exiting\! 1>&2
		exit 1
	fi
	# verify package instalation location
	if ! [ -d "${ROOT}/${REPO}" ]; then
		echo Somethings wrong, \"${ROOT}/${REPO}\" does not exist. 1>&2
		echo Maybe the \"${REPO}\" package files were removed manually. 1>&2
		echo Exiting\! 1>&2
		exit 2
	fi
	# clone or update
	cd "${ROOT}/${REPO}"
	if [ -d "${ROOT}/${REPO}/.git" ]; then
		echo Refreshing repo. 1>&2
		git pull
		git fetch --all
		git reset --hard origin/master
	else
		echo Cloning repo. 1>&2
		git clone --no-checkout "git@github.com:${GIT}/${REPO}.git" tmp
		mv tmp/.git .
		git reset --hard origin/master
		git stash src/clone-clixs.sh
		git pull
		git fetch --all
	fi
	# update deb package from src directory
	if ! [ -f "${ROOT}/${REPO}/src/latest" ]; then
		echo Missing \"${ROOT}/${REPO}/src/latest\"\! 1>&2
		echo Cannot attempt to update \"${REPO}\" deb package. 1>&2
		echo Exiting\! 1>&2
		exit 3
	fi
        echo PID :: $PID
        echo \$\$ :: $$
	ps              -o pid,ppid,cmd -p $$
	ps --no-heading -o pid,ppid,cmd -p $(ps --no-heading -o ppid -p $$)
	ps --no-heading -o pid,ppid,cmd -p $(ps --no-heading -o ppid -p $(ps --no-heading -o ppid -p $$))
	ps --no-heading -o pid,ppid,cmd -p $(ps --no-heading -o ppid -p $(ps --no-heading -o ppid -p $(ps --no-heading -o ppid -p $$)))
	local CURVER=$(dpkg -p clixs | awk '/^Version:/{print $2}')
	echo CURVER :: ${CURVER}
	local LATEST=$(echo $(cat "${ROOT}/${REPO}/src/latest")) 
	if [ "${CURVER}" == "${LATEST}" ]; then
		echo The most current version of the \"${REPO}\" package is already installed. 1>&2
		echo DONE 1>&2
		exit 0
	elif ! [ -f "${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb" ]; then
		echo The latest version of the \"${REPO}\" package is missing\; ${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb 1>&2
		echo Maybe the repo did not clone or update without error? 1>&2
		echo Exiting\! 1>&2
		exit 4
	else
		dpkg -i "${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb"
	fi
	
}
main "$@"

