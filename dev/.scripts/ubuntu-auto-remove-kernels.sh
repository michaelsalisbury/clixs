#!/bin/bash
function main(){
	[ "${1:0:2}" == "-y" ] && local YES='-y' || local YES=""
	apt-get ${YES} purge $(packages_to_remove)
	apt-get ${YES} install -f
}
function packages_to_remove(){
	local PACKAGE_LIST=$(dpkg -la)
	while read KERNEL; do
		awk "\$3 == \"${KERNEL}\" {print \$2}" <<< "${PACKAGE_LIST}"
	done < <(kernels_to_remove)
}
function kernels_installed(){
	dpkg -l 'linux-image-*' |
		awk '$1 == "ii" {print $3}' |
		sed -n '/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+-[[:digit:]]\+\.[[:digit:]]\+$/p' |
       		sort |
		uniq
}
function kernels_to_remove(){
	kernels_installed |
	grep -v -x -F -f <(kernels_to_keep)
}
function kernels_to_keep(){
	local RUNNING_KERNEL=$(uname -r | sed "s/\(.*\)-\([^0-9]\+\)/\1/")
	kernels_installed |
	grep -A100 -B1 "${RUNNING_KERNEL}"
}
main "$@"
exit $?
