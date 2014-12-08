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
		echo "Refreshing Repo"
		git reset --hard origin/master
		git pull
		git fetch --all
	else
		echo Cloning Repo
		git clone --no-checkout "git@github.com:${GIT}/${REPO}.git" tmp
		mv tmp/.git .
		git reset --hard origin/master
		git stash src/clone-clixs.sh
		git pull
		git fetch --all
	fi | enclose_text -t '~' -H -s "[GIT]"

	# update deb package from src directory
	if ! [ -f "${ROOT}/${REPO}/src/latest" ]; then
		echo Missing \"${ROOT}/${REPO}/src/latest\"\! 1>&2
		echo Cannot attempt to update \"${REPO}\" deb package. 1>&2
		echo Exiting\! 1>&2
		exit 3
	else
		local LATEST=$(echo $(cat "${ROOT}/${REPO}/src/latest"))
	fi

	if echo $(ps_reverse_tree $$ 1 --no-heading -o comm) |
	   grep -q "^$(basename "$0") ${REPO}.postinst dpkg"; then
		echo \"$(basename "$0")\" called from within a dpkg install process. 1>&2
		local CURVER=$(ps_reverse_tree $$ 4 --no-heading -o comm,cmd |
				awk '{if($1=="dpkg") print $NF}' |
				xargs basename -s .deb |
				sed "s/^${REPO}_//")
		if [ "${CURVER}" != "${LATEST}" ]; then
			echo There is a newer version of \"${REPO}\" package available. 1>&2
			echo Please update to version \"${LATEST}\". 1>&2
			if [ -f "${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb" ]; then
				echo It looks like the newer package has already been downloaded to... 1>&2
				echo \"${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb\" 1>&2
				echo please run \"sudo dpkg -i ${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb\" 1>&2
			elif [ -f "${ROOT}/${REPO}/src/clone-${REPO}.sh" ]; then
				echo Something is amiss, the newest package has not been cloned. 1>&2
				echo please run \"sudo ${ROOT}/${REPO}/src/clone-${REPO}.sh\" to rectify. 1>&2
			fi
			echo Exiting\!
			exit 4
		fi
	else
		local CURVER=$(dpkg -p clixs | awk '/^Version:/{print $2}')
	fi

	if [ "${CURVER}" == "${LATEST}" ]; then
		echo The most current version of the \"${REPO}\" package is already installed. 1>&2
		echo DONE 1>&2
		return 0
	elif ! [ -f "${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb" ]; then
		echo The latest version of the \"${REPO}\" package is missing\; ${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb 1>&2
		echo Maybe the repo did not clone or update without error? 1>&2
		echo Exiting\! 1>&2
		exit 5
	else
		dpkg -i "${ROOT}/${REPO}/src/${REPO}_${LATEST}.deb"
	fi
	
}
function enclose_text(){
	if [ -x "${ROOT}/bin/enclose.sh" ]; then
		"${ROOT}/bin/enclose.sh" "$@"
	else
		cat
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
exit $?

