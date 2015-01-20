#!/bin/bash

function includes(){
	# source variables;  DISTRIB_ID  DISTRIB_RELEASE  DISTRIB_CODENAME  DISTRIB_DESCRIPTION
	#	DISTRIB_ID=Ubuntu
	#	DISTRIB_RELEASE=12.10
	#	DISTRIB_CODENAME=quantal
	#	DISTRIB_DESCRIPTION="Ubuntu 12.10"

	source /etc/lsb-release
	#functions*.sh
	#../functions/functions*.sh
}

# GLOBAL VARIABLES
function global_variables(){ 
        [ "x86_64" == $(uname -i) ]\
                && MACHINE='amd64' \
                || MACHINE='i386'
	SHM="/dev/shm/$$${scriptName}$$"
	TMP="/tmp/$$${scriptName}$$"
	mkdir "${TMP}"
	mkdir "${SHM}"

	UID_MIN=1000
	UID_MAX=10000
	URL_BASE="http://download.virtualbox.org/virtualbox"
        LATEST=`wget -O - -o /dev/null ${URL_BASE}/LATEST.TXT`
	VERSION=${LATEST%.${LATEST#*.*.}}

	# setup VirtualBox Extension Pack url
	#		  Oracle_VM_VirtualBox_Extension_Pack-4.2.16.vbox-extpack
        EXTPACK_FILENAME="Oracle_VM_VirtualBox_Extension_Pack-${LATEST}.vbox-extpack"
	EXTPACK_URL="${URL_BASE}/${LATEST}/${EXTPACK_FILENAME}"

	# setup VirtualBox host deb url: crawl downloads for LATEST version
	local SPIDER="--spider -r -nd -l 1 --cut-dirs 1 -A deb"
	wget ${SPIDER} "${URL_BASE}/${LATEST}" 2> "${SHM}/${FUNCNAME}_spider"
	# setup VirtualBox host deb url: filter results for appropriate location
	local FILTER="^Location:.*virtualbox.${VERSION}.${LATEST}.[0-9]\+.${DISTRIB_ID}.${DISTRIB_CODENAME}.${MACHINE}"
	grep "${FILTER}" "${SHM}/${FUNCNAME}_spider" > "${SHM}/${FUNCNAME}_location"
	# setup VirtualBox host deb url: parse location for url
	VBOX_URL=`awk '{printf $2}' "${SHM}/${FUNCNAME}_location"`
	VBOX_FILENAME=${VBOX_URL##*\/}
	
}

function main(){
	includes
	global_variables
	echo ${VBOX_URL}
	echo ${VBOX_FILENAME}
	# get newest vbox version
	cd "${TMP}"
	wget -nv ${VBOX_URL}
	(( $? )) && echo Download Failed :: ${VBOX_URL} && EXIT 1
	wget -nv ${EXTPACK_URL}
	(( $? )) && echo Download Failed :: ${EXTPACK_URL} && EXIT 1
	ls -l
	# Install VirtualBox
	dpkg -i ${VBOX_FILENAME}

	# Install VirtualBox Extension Pack
	vboxmanage extpack uninstall "Oracle VM VirtualBox Extension Pack"
	vboxmanage extpack cleanup
	vboxmanage extpack install "${EXTPACK_FILENAME}"
	vboxmanage list extpacks

	EXIT 0
}
function EXIT(){
	rm -rf ${TMP}
	rm -rf ${SHM}
	exit $1
}
scriptFQFN=$(readlink -nf "${BASH_SOURCE}")
scriptName=$(basename "${scriptFQFN}")
scriptPath=$(dirname  "${scriptFQFN}")

main "$@"



