#!/bin/bash

while read disk; do
	DISK=$(basename "${disk}")
	SOURCE=$(find ${disk}/home/localcosadmin/.VirtualBox/HardDisks -maxdepth 0 -type d)
	MIRROR="/mnt/sdc3-UBUNTU_DATA0/backups/DLPBMM1/VirtualBox_HardDisks-${DISK}"
	#echo disk :: $disk
	#echo DISK :: $DISK
	#echo SOURCE :: $SOURCE
	#echo MIRROR :: $MIRROR
	mkdir -p "${MIRROR}"
	rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"
done < <(find /mnt/sshfs/DLPBMM1/sd[bc]1 -maxdepth 0 -type d)
#done < <(find /mnt/sshfs/DLPBMM1/sd[a-z]1/home/localcosadmin/.VirtualBox/HardDisks -maxdepth 0 -type d)
