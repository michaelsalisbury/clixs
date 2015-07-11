#!/bin/bash








#Local VMLINUZ INITRD zfsROOT

zfsROOT="rpool/ROOT/ubuntu-14.04/stable-0000-2015.06.01"
zfsBASE=$(dirname  "${zfsROOT}")
zfsNAME=$(basename "${zfsROOT}")
zfsDATE=${zfsNAME: -10}
zfsVERS=${zfsNAME%-${zfsDATE}}
zfsVERS=${zfsVERS: -4}
zfsNAME=${zfsNAME%${zfsNAME: -16}}

zfsVERS=000$(bc <<< "${zfsVERS} + 1")
zfsVERS=${zfsVERS: -4}

echo ${zfsBASE}
echo ${zfsNAME}
echo ${zfsVERS}
echo ${zfsDATE}

zfsSTOR="/boot/zfs/${zfsROOT}"

mkdir -p "${zfsSTOR}"
while read VMLINUZ; do
	VMLINUZ=$(basename "${VMLINUZ}")
	INITRD=${VMLINUZ/vmlinuz-/initrd.img-}
	if [ -d "${zfsSTOR}" ] &&
	   [ -f "/boot/${VMLINUZ}" ] &&
	   [ -f "/boot/${INITRD}" ]; then
		if ! [ -f "${zfsSTOR}/${VMLINUZ}" ]; then
			ln -vf "/boot/${VMLINUZ}" "${zfsSTOR}/".
		fi
		if [ -f "${zfsSTOR}/${INITRD}" ]; then
			# test if initrd.img is new and increment accordingly
			#if ! diff "/boot/${INITRD}" "${zfsSTOR}/${INITRD}" &> /dev/null; then
				INITRD_LAST=$(ls -1 "${zfsSTOR}/${INITRD}".v* | tail -1)
				INITRD_VERS=${INITRD_LAST: -2}
				INITRD_VERS=0$(bc <<< "${INITRD_VERS} + 1")
				INITRD_VERS=${INITRD_VERS: -2}
				INITRD_NEW="${zfsSTOR}/${INITRD}".v${INITRD_VERS:-00}
				mv -v "${zfsSTOR}/${INITRD}"{,.v${INITRD_VERS:-00}}
				ln -vf "/boot/${INITRD}"  "${zfsSTOR}/".
			#fi
		else
			ln -vf "/boot/${INITRD}"  "${zfsSTOR}/".
		fi
	fi
done < <(ls -1 /boot/vmlinuz*)



