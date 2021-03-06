#!/bin/bash


name="Kickstart Test RHEL"
vrdeport='3392'

VBoxManage showvminfo "${name}"

#VBoxManage modifyvm   "${name}" --boot1 net --boot2 disk --boot3 dvd --boot4 none
VBoxManage modifyvm   "${name}" --boot1 disk --boot2 net --boot3 dvd --boot4 none
VBoxManage modifyvm   "${name}" --nic1 bridged --cableconnected1 on --bridgeadapter1 eth2 --nictype1 82540EM
VBoxManage modifyvm   "${name}" --vrde on --vrdeport ${vrdeport} --vrdeauthtype null --vrdemulticon on



exit 0

VBoxManage createvm --name ${name}.${disk} --ostype Other --register
VBoxManage modifyvm        ${name}.${disk} --memory ${mem}
VBoxManage modifyvm        ${name}.${disk} --boot1 dvd --boot2 net --boot3 disk --boot4 none
VBoxManage modifyvm        ${name}.${disk} --nic1 bridged --cableconnected1 on --bridgeadapter1 eth1 --nictype1 82540EM --macaddress1 ${macaddress}
VBoxManage modifyvm        ${name}.${disk} --vrde on --vrdeport ${vrdeport} --vrdeauthtype null --vrdemulticon on

VBoxManage storagectl      ${name}.${disk} --name ${sctl} --add ${sctl} --bootable on
VBoxManage storageattach   ${name}.${disk} --storagectl ${sctl} --port 0 --device 0 --type dvddrive --medium \"${iso}\"
VBoxManage storageattach   ${name}.${disk} --storagectl ${sctl} --port 1 --device 0 --type hdd --medium \"${vmdk}\"

# Diagnostic
#su $username -c "/usr/bin/gnome-terminal --maximize --working-directory=\"${userhome}\" -e \"bash -c 'VBoxManage showvminfo ${name}.${disk} && sleep 120'\"" &

su $username -c "VirtualBox --startvm ${name}.${disk}"  &

echo "Hello World --- $disk @ $username : $name --- $(date)" >> /root/.custApps/udev.out







exit 0

scriptName="$(basename $BASH_SOURCE)"
scriptPath="$(cd `dirname  $BASH_SOURCE`; pwd)"

username=$(who -u | grep "(:0)" | cut -f1 -d" ")
userhome="$(cat /etc/passwd | grep $username | cut -f6 -d:)"

disk=$1
iso=/home/localcosadmin/ISO/SpinRite.iso
mem=128
jobdetailsh="${scriptPath}/udev.query.sh"
jobdetails="${userhome}/.VirtualBox/jobDetails.$disk"

set -x
xhost local:${username}
export DISPLAY=:0.0

su $username -c "/usr/bin/gnome-terminal --working-directory=\"$userhome\" -e \"${jobdetailsh} ${disk} $$\""
# importing this file include the following variables : name, 
. ${jobdetails}
# this is important because if more than one gnome-terminal window is open this script will continue before the user has enter job details
while [ "$jobp" != "$$" ]; do
	let "counter+=1"
	[ $counter -gt 30 ] && exit 0
	sleep 1
	. ${jobdetails}
done


vrdeport=$(( 33890 + $(printf "%d\n" \'${disk:2}) - 99 ))
macaddress=080027ABCD$(( 196 - $(printf "%d\n" \'${disk:2}) ))
name="${name}-${vrdeport}"
vmdk="${userhome}/.VirtualBox/udev.${disk}.${name}.vmdk"
sctl=ide


ls -lha "${userhome}/.VirtualBox/udev.${disk}".*.vmdk
rm -f   "${userhome}/.VirtualBox/udev.${disk}".*.vmdk
su $username -c "VBoxManage unregistervm \$(VBoxManage list vms | grep ${disk} | sed 's/^.//;s/. .*//') --delete"

VBoxManage internalcommands createrawvmdk -filename "${vmdk}" -rawdisk /dev/${disk}
chmod a+rw ${vmdk}
chown $username.vboxusers ${vmdk}
chmod a+rw /dev/${disk}
chown $username.vboxusers /dev/${disk}


ls -lha "${userhome}/VirtualBox VMs"/*.${disk}
rm -rf  "${userhome}/VirtualBox VMs"/*.${disk}
su $username -c "VBoxManage createvm --name ${name}.${disk} --ostype Other --register"
su $username -c "VBoxManage modifyvm        ${name}.${disk} --memory ${mem}"
su $username -c "VBoxManage modifyvm        ${name}.${disk} --boot1 dvd --boot2 net --boot3 disk --boot4 none"
su $username -c "VBoxManage modifyvm        ${name}.${disk} --nic1 bridged --cableconnected1 on --bridgeadapter1 eth1 --nictype1 82540EM --macaddress1 ${macaddress}"
su $username -c "VBoxManage modifyvm        ${name}.${disk} --vrde on --vrdeport ${vrdeport} --vrdeauthtype null --vrdemulticon on"

su $username -c "VBoxManage storagectl      ${name}.${disk} --name ${sctl} --add ${sctl} --bootable on"
su $username -c "VBoxManage storageattach   ${name}.${disk} --storagectl ${sctl} --port 0 --device 0 --type dvddrive --medium \"${iso}\""
su $username -c "VBoxManage storageattach   ${name}.${disk} --storagectl ${sctl} --port 1 --device 0 --type hdd --medium \"${vmdk}\""

# Diagnostic
#su $username -c "/usr/bin/gnome-terminal --maximize --working-directory=\"${userhome}\" -e \"bash -c 'VBoxManage showvminfo ${name}.${disk} && sleep 120'\"" &

su $username -c "VirtualBox --startvm ${name}.${disk}"  &

echo "Hello World --- $disk @ $username : $name --- $(date)" >> /root/.custApps/udev.out
