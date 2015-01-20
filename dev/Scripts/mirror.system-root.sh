#!/bin/bash

#MIRROR='/mnt/sdb3-UBUNTU_MIRROR'
MIRROR='/mnt/md0'
MYBOOK='/MyBook/System_G88192RMXYK'
#rsync -vaEhPu --inplace \
rsync -vaEhPu --inplace \
	--exclude=/mnt\
	--exclude=/proc\
	--exclude=/sys\
	--exclude=/dev\
	--exclude=/data\
	--exclude=/MyBook\
	--exclude=/boot/efi-sde1\
	--exclude=/etc/fstab\
	--exclude=/etc/rc.local\
	--include=*.vdi\
	--include=*.vmdk\
	--include=*.dd\
	--include=*.raw\
	/ "${MIRROR}"

rsync -vaEhPu \
	--delete-before\
	--exclude=/mnt\
	--exclude=/proc\
	--exclude=/sys\
	--exclude=/dev\
	--exclude=/data\
	--exclude=/MyBook\
	--exclude=/boot/efi-sde1\
	--exclude=/etc/fstab\
	--exclude=/etc/rc.local\
	--exclude=*.vdi\
	--exclude=*.vmdk\
	--exclude=*.dd\
	--exclude=*.raw\
	/ "${MIRROR}"

exit

rsync -vaEhPu --inplace \
	--exclude=/mnt\
	--exclude=/proc\
	--exclude=/sys\
	--exclude=/dev\
	--exclude=/data\
	--exclude=/MyBook\
	--exclude=/boot/efi-sde1\
	--include=*.vdi\
	--include=*.vmdk\
	--include=*.dd\
	--include=*.raw\
	/ "${MYBOOK}"

rsync -vaEhPu \
	--delete-before\
	--exclude=/mnt\
	--exclude=/proc\
	--exclude=/sys\
	--exclude=/dev\
	--exclude=/data\
	--exclude=/MyBook\
	--exclude=/boot/efi-sde1\
	--exclude=*.vdi\
	--exclude=*.vmdk\
	--exclude=*.dd\
	--exclude=*.raw\
	/ "${MYBOOK}"

