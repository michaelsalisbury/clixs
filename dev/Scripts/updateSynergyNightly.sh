#!/bin/bash
#wget    --recursive                             \
#        --timestamping                          \
#        --no-directories                        \
#        --level 1                               \
#        --cut-dirs 1                            \
#        --accept gz				\
#        --directory-prefix /home/localcosadmin/Downloads/Synergy/""	\
#	https://synergy-project.org/nightly
#exit


for package_type in gz msi deb rpm dmg; do
mkdir -p /home/localcosadmin/Downloads/Synergy/${package_type#gz}
wget	--recursive				\
	--timestamping				\
	--no-directories			\
	--level 1				\
	--cut-dirs 1				\
	--accept ${package_type}		\
        --directory-prefix /home/localcosadmin/Downloads/Synergy/${package_type#gz}\
	https://synergy-project.org/nightly
done
