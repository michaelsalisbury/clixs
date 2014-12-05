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
	if echo $(ps_reverse_tree $$ 1 --no-heading -o comm) |
	   grep "^$(basename "$0") ${REPO}.postinst dpkg"; then
		echo \"$(basename "$0")\" called from within dpkg. 1>&2
		ps_reverse_tree $$ 4 -o pid,ppid,comm,cmd	
		#echo No need to \! 1>&2
		return
	elif ! [ -f "${ROOT}/${REPO}/src/latest" ]; then
		echo Missing \"${ROOT}/${REPO}/src/latest\"\! 1>&2
		echo Cannot attempt to update \"${REPO}\" deb package. 1>&2
		echo Exiting\! 1>&2
		exit 3
	fi
	
	local CURVER=$(dpkg -p clixs | awk '/^Version:/{print $2}')
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
function ps_reverse_tree(){
	local CHILD=${1:-$$}
	local DESCEND=${2:-2}
	(( DESCEND == 1 )) && (( DESCEND-=1 ))
	local PARENT
	shift 2
	if (( ${#@} )); then
		local OPTS=$*
	else
		local OPTS="-o pid,ppid,comm,cmd"
	fi
	ps ${OPTS} -p ${CHILD}
	while (( DESCEND-=1 )); do
		PARENT=$(ps --no-heading -o ppid -p ${PARENT:-${CHILD}})
		ps --no-heading ${OPTS/--no-heading/} -p ${PARENT}
		(( PARENT == 1 )) && break
	done

}
main "$@"

