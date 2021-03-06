#!/bin/builder.sh
skip=( false false false false false false false )
step=1
prefix="setup"
source=http://10.173.119.78/kickstart/config/${scriptName}

customName=builder

function setup_Prep(){
	desc Prep: Wipe sda\! \& extract
	# Setup /dev/sda
	sfdisk    /dev/sda << END-OF-SFDISK
;
END-OF-SFDISK
	mkfs.ext3 /dev/sda1
	mkdir               /mnt/custom
	mount     /dev/sda1 /mnt/custom
	du --max-depth=1 -h /mnt/custom
	#
	sysresccd-custom extract
	
}
function setup_chroot(){
	desc chroot to /mnt/custom/customcd/files/
	mount -t proc proc   /mnt/custom/customcd/files/proc/
	mount -t sysfs sys   /mnt/custom/customcd/files/sys/
	mount -o bind /dev   /mnt/custom/customcd/files/dev/
	cat /etc/mtab | grep /mnt/custom/customcd/files
	terminal --maximize -x /bin/bash -c "/bin/chroot /mnt/custom/customcd/files"
	umount /mnt/custom/customcd/files/dev/
	umount /mnt/custom/customcd/files/sys/
	umount /mnt/custom/customcd/files/proc/
	cat /etc/mtab | grep /mnt/custom/customcd/files
}
function setup_Customize(){
	desc Customize: Bash
	# Bash Config + PS1
}
function setup_Pack_squashfs(){
	desc Pack squashfs
	sysresccd-custom squashfs
}
function setup_Pack_ISO(){
	desc Pack ISO
	rm -f /mnt/custom/customcd/isofile/*
	sysresccd-custom setkeymap en
	local version=`cat /livemnt/boot/version`
	sysresccd-custom isogen systemrescuecd-x86-${version}-${customName}
	cd /mnt/custom/customcd/isofile
	for file in `ls`; do
		mv $file systemrescuecd-x86-${version}-${customName}.${file##*.}
	done
	mv /mnt/custom/customcd/isofile/* /mnt/custom/.
}
function setup_vboxadditions(){
        desc Install Virtual Box Linux Additions
        ###################################################################################
        mkdir /mnt/custom/Downloads
        cd    /mnt/custom/Downloads
        rm -f /mnt/custom/Downloads/LATEST.TXT
	rm -f /mnt/custom/Downloads/SHA256SUMS
	rm -f /mnt/custom/Downloads/*.run
        wget -nv http://download.virtualbox.org/virtualbox/LATEST.TXT
        cat   /mnt/custom/Downloads/LATEST.TXT
        local version=`cat /mnt/custom/Downloads/LATEST.TXT`
	wget -nv http://download.virtualbox.org/virtualbox/${version}/SHA256SUMS
	cat /mnt/custom/Downloads/SHA256SUMS | grep run

		#--progress=dot:meta
	wget    --timestamping                          	\
		--recursive					\
		--no-verbose					\
	        --no-directories                        	\
	        --level 1                               	\
	        --cut-dirs 2                            	\
	        --accept run					\
	        --directory-prefix /mnt/custom/Downloads	\
	        http://download.virtualbox.org/virtualbox/${version}/

	
        #rm -f /mnt/custom/Downloads/VBoxGuestAdditions_${version}.iso
        #wget -nv http://download.virtualbox.org/virtualbox/${version}/VBoxGuestAdditions_${version}.iso
        #umount                                                        /mnt/custom/Downloads/vbox_guest_additions
        #mount -t iso9660 -o loop,ro VBoxGuestAdditions_${version}.iso /mnt/custom/Downloads/vbox_guest_additions
        #cd    /root/Downloads/vbox_guest_additions
        #./VBoxLinuxAdditions.run
        #cd    /root/Downloads
        #umount                                                        /root/Downloads/vbox_guest_additions
        #reboot
}

