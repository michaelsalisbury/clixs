#!/bin/builder.sh
skip=( false false )
step=1
prefix="setup"
source=http://192.168.248.24/config/$scriptName

sourceI="${HOME}/ubuntu-12.10-desktop-amd64.iso"
workDir="${HOME}/uck_vboxclient"
scriptD="${workDir}/customization-scripts"


function setup_VirtualBox_Client_Image(){
	desc Setup-UP Working Directory \& Unpack
	mkdir -p "${workDir}"
	mkdir -p "${scriptD}"
	sudo uck-remaster-clean-all               "${workDir}"
	sudo uck-remaster-unpack-iso "${sourceI}" "${workDir}"
}
function setup_Unpack_initrd(){
	desc Unpack initrd
	sudo uck-remaster-unpack-initrd "${workDir}"
}
function setup_Unpack_rootfs(){
	desc Unpack rootfs
	sudo uck-remaster-unpack-rootfs "${workDir}"
}
function setup_chootfs(){
	desc Run test in chroot file system
	local cmd="test.sh"
	cp -f "${buildScriptFQFN}" "${scriptD}/${buildScriptName}"
	touch                      "${scriptD}/${cmd}"
	chmod +x                   "${scriptD}/${cmd}"
	cat << END-OF-SCRIPT >     "${scriptD}/${cmd}"
#!/bin/bash
echo hello
apt-get update
END-OF-SCRIPT
	sudo uck-remaster-chroot-rootfs "${workDir}" "/tmp/customization-scripts/${cmd}"
}
