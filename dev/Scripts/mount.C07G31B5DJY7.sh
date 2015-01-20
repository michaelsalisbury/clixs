#!/bin/bash

username=$(whoami)
uid=$(cat /etc/passwd | grep ${username} | cut -d: -f3)
gid=$(cat /etc/passwd | grep ${username} | cut -d: -f4)
home=$(cat /etc/passwd | grep ${username} | cut -d: -f6)
groupname=$(cat /etc/group | grep ${gid} | cut -d: -f1)



#uid='localcosadmin'
#gid='localcosadmin'
#user='localcosadmin'
user=${username}
pass=
domain='cos.ucf.edu'
server='c07G31B5DJY7'
fqdn="${server}.${domain}"

opt="rw,user=${username},group=${groupname}"

shares='Backups DeployStudioNFS Groups NetBootClients0 Public Users localcosadmin'
shares='Backups DeployStudioNFS localcosadmin'


# List shares by trying to mount a bad share name 
sudo mount_afp -o "${opt}" "afp://${user}:${pass}@${fqdn}/List-Volumes" .

# Mount AFP shares define above
for share in ${shares}; do
	sudo mkdir -pv ${home}/${domain}/${server}_${share}
	sudo mount_afp -o "${opt}" "afp://${user}:${pass}@${fqdn}/${share}" "${home}/${domain}/${server}_${share}"
done

