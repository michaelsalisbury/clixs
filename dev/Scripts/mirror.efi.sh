#!/bin/bash

MIRROR='/boot/efi/EFI/images/ubuntu-13.04-desktop-x86_64'

SOURCE='/boot/initrd.img*'
#rsync ${SOURCE} ${MIRROR}/.

SOURCE='/boot/vmlinuz*'
#rsync ${SOURCE} ${MIRROR}/.

SOURCE='/boot/efi/'

MIRROR='/boot/efi-sdb1/'
rsync	-vaxEhPu 		\
	--delete-before		\
	--exclude=refind_x64.*	\
	${SOURCE} ${MIRROR}

MIRROR='/boot/efi-sdc1/'
rsync	-vaxEhPu 		\
	--delete-before		\
	--exclude=refind_x64.*	\
	${SOURCE} ${MIRROR}

MIRROR='/boot/efi-sdd1/'
rsync	-vaxEhPu 		\
	--delete-before		\
	--exclude=refind_x64.*	\
	${SOURCE} ${MIRROR}







exit

SOURCE='/mnt/sdc3-UBUNTU_DATA0/'
MIRROR='/mnt/sdd3-UBUNTU_DATA1/'
MYBOOK='/mnt/sde1-MyBook/'
#rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"
rsync -vaxEhPu --delete-before "${SOURCE}"       "${MIRROR}"
rsync -vaxEhPu --delete-before "${SOURCE}/data/" "${MYBOOK}/Data/"

