#!/bin/builder.sh
skip=( false false false false false false false false false false false false false )
step=1
prefix="setup"
source=http://10.173.119.78/kickstart/config/${scriptName}

function setup_Config_OwnCloud_Update_Script(){
	#################################################################################################
	################################################################## /etc/owncloud/updateScript.sh
	desc 80 Write Update Script to /etc/owncloud/updateScript.sh
	mkdir     /etc/owncloud
	cd        /etc/owncloud
	wget http://10.173.119.78/packages/owncloud/updateScript.sh
	chmod 755 /etc/owncloud/updateScript.sh
	          /etc/owncloud/updateScript.sh
}
function setup_Enable_Verbose_Boot(){
	#################################################################################################
	################################################################## /boot/grub/grub.conf
	desc 80 Remove \"rhgb\" and \"quiet\" options from gub
	sed 's/rhgb quiet/noquiet/' /boot/grub/grub.conf
}
function setup_Config_sshd(){
	desc 80 /etc/sshd/sshd_config
	sed -i "s/.*GSSAPIAuthentication yes.*/#GSSAPIAuthentication yes/" /etc/ssh/sshd_config
	/sbin/service sshd restart
}
function setup_Config_ntpd(){
	desc 80 /etc/ntp.conf
	/sbin/service     ntpd stop
	/usr/sbin/ntpdate ntp.myfloridacity.us
	/sbin/chkconfig   ntpd on
	/sbin/service     ntpd start
}
function setup_Install_YUM(){
	desc 80 Setup Repos \for YUM, \run Updates and Reboot
	# EPEL repo has inotify-tools and incron
	rpm -i http://mirror.ancl.hawaii.edu/linux/epel/6/i386/epel-release-6-7.noarch.rpm
	# Webtatic repo has php5.4
	#rpm -Uvh http://repo.webtatic.com/yum/el6/latest.rpm
	# Alt repo has php5.3.14, apache 2.2.22, mysql 5.5.x
	rpm -Uvh http://centos.alt.ru/repository/centos/6/x86_64/centalt-release-6-1.noarch.rpm
	yum -y update
	reboot
}

function setup_Install_VBox_GuestAdditions(){
        desc 80 Virtual Box GuestAdditions
        yum -y install kernel-devel kernel-headers dkms gcc make
        mkdir /root/Downloads
        mkdir /root/Downloads/vbox_guest_additions
        cd    /root/Downloads
        rm -f /root/Downloads/LATEST.TXT
        wget -nv http://download.virtualbox.org/virtualbox/LATEST.TXT
        cat   /root/Downloads/LATEST.TXT
        local version='4.1.16'
        local version='4.1.18'
        local version=`cat /root/Downloads/LATEST.TXT`
        rm -f /root/Downloads/VBoxGuestAdditions_${version}.iso
        wget -nv http://download.virtualbox.org/virtualbox/${version}/VBoxGuestAdditions_${version}.iso
        umount                                                        /root/Downloads/vbox_guest_additions
        mount -t iso9660 -o loop,ro VBoxGuestAdditions_${version}.iso /root/Downloads/vbox_guest_additions
        cd    /root/Downloads/vbox_guest_additions
        ./VBoxLinuxAdditions.run
        cd    /root/Downloads
        umount                                                        /root/Downloads/vbox_guest_additions
	reboot
}
function setup_OwnCloud_Prep_YUM_Prereqs(){
	desc 80 php-mbstring libcurl-devel php-pecl-zip php-mysql php-sqlite
		# Required packages... apache2 php5 php5-json php-xml php-mbstring php5-zip php5-gd
		#   php5-json, php5-zip provided by php-common
		#   
		# Required packages... php5-sqlite curl libcurl3 libcurl3-dev php5-curl php-pdo
		#   php5-curl is provided by php-common
		#   php5-sqlite is provided by php-pdo
		yum -y install php-mbstring libcurl-devel php-pecl-zip php-mysql php-sqlite
}
function setup_OwnCloud_Prep_MySQL(){
	desc 80 secure mysql
	chkconfig --level 235 mysqld on
	service mysqld start

	mysql_root_pass='1qaz@WSX'
	mysql_user_pass='1qaz@WSX'
	mysqladmin -u root password "${mysql_root_pass}"
	mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
	        DROP DATABASE IF EXISTS test;
	        DROP DATABASE IF EXISTS owncloud_git;
	        DROP DATABASE IF EXISTS owncloud;"
	mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
	        DELETE FROM mysql.user WHERE host='localhost'  AND user<>'root';
       		DELETE FROM mysql.user WHERE                       user='owncloud';
       		DELETE FROM mysql.user WHERE host<>'localhost' AND host<>'127.0.0.1';"
	mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
	        CREATE DATABASE owncloud;
	        CREATE DATABASE owncloud_git;
	        GRANT ALL on owncloud_git.* TO 'owncloud'@'localhost' IDENTIFIED BY '"${mysql_user_pass}"';
	        GRANT ALL on owncloud_git.* TO 'owncloud'@'127.0.0.1' IDENTIFIED BY '"${mysql_user_pass}"';
	        GRANT ALL on owncloud.*     TO 'owncloud'@'localhost' IDENTIFIED BY '"${mysql_user_pass}"';
	        GRANT ALL on owncloud.*     TO 'owncloud'@'127.0.0.1' IDENTIFIED BY '"${mysql_user_pass}"';"
	mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
	        FLUSH PRIVILEGES;"
	echo
        mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
                SHOW DATABASES;"                                              \
                | sed "1a`repc 80 -`"
        echo
        mysql -u root --password="${mysql_root_pass}" -h localhost --execute="\
                SELECT user,host,password FROM mysql.user;"                   \
                | column -t                                                   \
                | sed "1a`repc 80 -`"
        echo
}
function setup_OwnCloud_Prep_Iptables(){
	desc 80 Iptables firewall rules per HTTP and HTTPS
	sed -i  '/22/{p;s/22/80/;p;s/80/443/}' /etc/sysconfig/iptables
	service iptables restart
}
function setup_OwnCloud_Prep_Postfix(){
	desc 80 Setup postfix to relay mail
	echo relayhost = ucfsmtps1.mail.ucf.edu >> /etc/postfix/main.cf
	echo smtp_use_tls = yes                 >> /etc/postfix/main.cf
	echo smtp_sasl_security_options =       >> /etc/postfix/main.cf
	tail -4					   /etc/postfix/main.cf
}
function setup_Webmin(){
	desc 80 Setup Webmin with wbm-sysstats on port 10000
	cat > /etc/yum.repos.d/webmin.repo << EOF
[Webmin]
name=Webmin Distribution Neutral
#baseurl=http://download.webmin.com/download/yum
mirrorlist=http://download.webmin.com/download/yum/mirrorlist
enabled=1
EOF
	mkdir /root/Downloads
	cd    /root/Downloads
	wget ftp://ftp.pbone.net/mirror/ftp.sourceforge.net/pub/sourceforge/w/we/webminstats/Sysstats/2.5/wbm-sysstats-2.5-1centos.noarch.rpm
	rpm --import http://www.webmin.com/jcameron-key.asc
	yum -y install  openssl openssl-devel	\
			rrdtool			\
			smartmontools		\
			perl-CPAN* perl-rrdtool perl-Crypt-SSLeay perl-Net-SSLeay perl-CGI
	rpm -ivh wbm-sysstats-2.5-1centos.noarch.rpm
	yum -y install webmin
}
function setup_Disable_RunOnce(){
	desc 80 Remove from rc.local
	sed -i "/ownCloud.SystemPrep.sh/s/^/#/" /etc/rc.d/rc.local
	cat                                     /etc/rc.d/rc.local
}

