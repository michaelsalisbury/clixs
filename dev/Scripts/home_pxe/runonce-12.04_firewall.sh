#!/bin/bash
scriptName="$(basename $BASH_SOURCE)"
scriptPath="$(cd `dirname  $BASH_SOURCE`; pwd)"

username=$(who -u | grep "(:" | head -1 | cut -f1 -d" ")
[ -z "$username" ] && username=root
userhome="$(cat /etc/passwd | grep $username | cut -f6 -d:)"

step=1
function next(){ sed -i".bk" "/^step=/s/$step/$(( step + 1))/" "$scriptPath"/"$scriptName"; }
function rset(){ sed -i".bk" "/^step=/s/$step/1/"              "$scriptPath"/"$scriptName"; }
function repc(){ echo `seq $1` | sed "s/ /$2/g;s/[^$2]//g"; }
function desc(){ echo; line="#### $@ $(repc 100 '#')"; echo ${line:0:100}; echo; }
function waitAptgetUpdate(){
        lockTestFile="/var/lib/apt/lists/lock"
        timestamp=$(date +%s)
        pso="-o pid,user,ppid,pcpu,pmem,cmd"
        if [ -n "$(lsof -t ${lockTestFile})" ]; then
                desc "Waiting on apt-get to finish in another process"
        fi
        while [ -n "$(lsof -t ${lockTestFile})" ]; do
                ps ${pso}                                           -p $(lsof -t ${lockTestFile})
                ps ${pso} --no-heading -p $(ps --no-heading -o ppid -p $(lsof -t ${lockTestFile}))
                if (( $(date +%s) - ${timestamp} > 120 )); then break; fi
                sleep 1
                echo $(( $(date +%s) - ${timestamp} )) :: Seconds Elapsed
        done
}
function waitAptgetInstall(){
        lockTestFile="/var/lib/dpkg/lock"
        timestamp=$(date +%s)
        pso="-o pid,user,ppid,pcpu,pmem,cmd"
        if [ -n "$(lsof -t ${lockTestFile})" ]; then
                desc "Waiting on apt-get to finish in another process"
        fi
        while [ -n "$(lsof -t ${lockTestFile})" ]; do
                ps ${pso}                                           -p $(lsof -t ${lockTestFile})
                ps ${pso} --no-heading -p $(ps --no-heading -o ppid -p $(lsof -t ${lockTestFile}))
                if (( $(date +%s) - ${timestamp} > 120 )); then break; fi
                sleep 1
                echo $(( $(date +%s) - ${timestamp} )) :: Seconds Elapsed
        done
}

aptopt="-y -q --force-yes"
autoLoginUser="msalisbury"

echo '###################################################################################################'
echo '###################################################################################################'
echo "#### Step[$step]"
echo
case $step in
        1)      desc "Modify /etc/ssh/sshd_config"
                sed -i "s/.*GSSAPIAuthentication yes.*/#GSSAPIAuthentication yes/" /etc/ssh/sshd_config
                stop ssh
                for s in `seq 20 -1 1`; do echo -n '.'; done; echo
                start ssh
                #/etc/init.d/ssh restart
                ###################################################################################
                desc "Setup Auto Login for ${autoLoginUser}"
                cat << EOF > /etc/lightdm/lightdm.conf
[SeatDefaults]
autologin-guest=false
autologin-user=${autoLoginUser}
autologin-user-timeout=10
autologin-session=lightdm-autologin
greeter-session=unity-greeter
user-session=ubuntu
EOF
                cat << EOF > /etc/lightdm/lightdm.template
[SeatDefaults]
autologin-guest=false
autologin-user=${autoLoginUser}
autologin-user-timeout=0
autologin-session=lightdm-autologin
greeter-session=unity-greeter
user-session=ubuntu

#user-session=ubuntu-2d
#user-session=xfce
#user-session=xsession
#user-session=xubuntu
#user-session=gnome
#user-session=gnome-shell
#user-session=gnome-classic
#user-session=gnome-fallback
EOF
                ###################################################################################
                desc "Setup Global GUI login script for all users"
                cat << EOF > /etc/bash.gui_login_system
gconftool-2 --set --type bool /apps/gnome-terminal/profiles/Default/login_shell true
EOF
                cat << EOF > /etc/xdg/autostart/bash.gui_login_system.desktop
[Desktop Entry]
Type=Application
Exec=bash /etc/bash.gui_login_system
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
Name[en_US]=BashGUILoginSystemScripts
Name=BashGUILoginSystemScripts
Comment[en_US]=Bash GUI Login System Script
Comment=Bash GUI Login System Script
EOF
                cat << EOF > /etc/profile.d/aliases.sh
alias ll='ls -la --color'
EOF
                ###################################################################################
                desc "Setup autostart terminals to display logs; runonce, syslog and top"
                su ${autoLoginUser} -c "mkdir -p /home/${autoLoginUser}/.config/autostart"
                su ${autoLoginUser} -c "touch /home/${autoLoginUser}/.bash_history"
                (cat << EOF
                cat << END-OF-AUTOSTART > /home/${autoLoginUser}/.config/autostart/syslog.desktop
[Desktop Entry]
Type=Application
Exec=gnome-terminal --geometry=80x30+0+1000 -e "tail -f /var/log/syslog"
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
Name[en_US]=syslog
Name=syslog
Comment[en_US]=Tail syslog
Comment=Tail syslog
END-OF-AUTOSTART
EOF
) | su ${autoLoginUser}
                (cat << EOF
                cat << END-OF-AUTOSTART > /home/${autoLoginUser}/.config/autostart/runonce.desktop
[Desktop Entry]
Type=Application
Exec=gnome-terminal --geometry=80x30+0+0 -e "tail -f /var/log/$scriptName"
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
Name[en_US]=$scriptName
Name=$scriptName
Comment[en_US]=Tail $scriptName
Comment=Tail $scriptName
END-OF-AUTOSTART
EOF
) | su ${autoLoginUser}
                (cat << EOF
                cat << END-OF-AUTOSTART > /home/${autoLoginUser}/.config/autostart/top.desktop
[Desktop Entry]
Type=Application
Exec=gnome-terminal --geometry=80x50+1000 -e top
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
Name[en_US]=top
Name=top
Comment[en_US]=top
Comment=top
END-OF-AUTOSTART
EOF
) | su ${autoLoginUser}
                (cat << EOF
                cat << END-OF-AUTOSTART > /home/${autoLoginUser}/.config/autostart/bash_gui_login_user.desktop
[Desktop Entry]
Type=Application
Exec=bash /home/${autoLoginUser}/.bash_gui_login_user
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
Name[en_US]=BashGUILoginScript
Name=BashGUILoginScript
Comment[en_US]=Bash GUI Login Script
Comment=Bash GUI Login Script
END-OF-AUTOSTART
EOF
) | su ${autoLoginUser}
                (cat << EOF
                cat << END-OF-AUTOSTART > /home/${autoLoginUser}/.bash_gui_login_user
gsettings set org.gnome.desktop.screensaver lock-enabled false
gsettings set org.gnome.desktop.screensaver ubuntu-lock-on-suspend false
gsettings set org.gnome.desktop.session idle-delay 0
END-OF-AUTOSTART
EOF
) | su ${autoLoginUser}
                (cat << EOF
                cat << END-OF-TOPRC > /home/${autoLoginUser}/.toprc
RCfile for "top with windows"           # shameless braggin'
Id:a, Mode_altscr=0, Mode_irixps=1, Delay_time=3.000, Curwin=0
Def     fieldscur=AEHIOQTWKNMbcdfgjplrsuvyzX
        winflags=32569, sortindx=10, maxtasks=0
        summclr=1, msgsclr=1, headclr=3, taskclr=1
Job     fieldscur=ABcefgjlrstuvyzMKNHIWOPQDX
        winflags=62777, sortindx=0, maxtasks=0
        summclr=6, msgsclr=6, headclr=7, taskclr=6
Mem     fieldscur=ANOPQRSTUVbcdefgjlmyzWHIKX
        winflags=62777, sortindx=13, maxtasks=0
        summclr=5, msgsclr=5, headclr=4, taskclr=5
Usr     fieldscur=ABDECGfhijlopqrstuvyzMKNWX
        winflags=62777, sortindx=4, maxtasks=0
        summclr=3, msgsclr=3, headclr=2, taskclr=3
END-OF-TOPRC
EOF
) | su ${autoLoginUser}
                ###################################################################################
                desc "apt-get clean"
                apt-get clean
                #apt-get ${aptopt} install python-gmenu
                next
                reboot
                exit 0
                ;;
        2)      desc "Disable Autostart Update-Notifier"
                su ${autoLoginUser} -c "cp /etc/xdg/autostart/update-notifier.desktop /home/${autoLoginUser}/.config/autostart/."
                sed -i '/X-GNOME-Autostart-Delay=/s/^.*$/X-GNOME-Autostart-enabled=false/' \
                        /home/${autoLoginUser}/.config/autostart/update-notifier.desktop
                #su ${autoLoginUser} -c "killall update-notifier"
                ###################################################################################
                desc "Clean out all Updates Notifications from /var/lib/update-notifier/..."
                mkdir -p                               /root/update-notifier
                rsync -axEhu /var/lib/update-notifier/ /root/update-notifier/
                rm -f        /var/lib/update-notifier/user.d/*
                rm -f        /var/lib/update-notifier/dpkg-run-stamp/*
                rm -f        /var/lib/update-notifier/package-data-downloads/*
                ###################################################################################
                desc "Run updates and upgrade"
                apt-get ${aptopt} update
                apt-get ${aptopt} upgrade
                next
                reboot
                exit 0
                ;;
        3)      desc "apt-get updates and reboot"
                for s in `seq 20 -1 1`; do echo -n "$s "; done; echo
                apt-get ${aptopt} update
                apt-get ${aptopt} upgrade
                ###################################################################################
                desc "remove gnaome-colors-common"
                apt-get ${aptopt} remove gnome-colors-common
                ###################################################################################
                desc "Prep for EULA and other apt-get prompts"
                echo hddtemp hddtemp/daemon select false | debconf-set-selections
                echo acroread-common acroread-common/default-viewer select true | debconf-set-selections
                echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true | debconf-set-selections
                ###################################################################################
                desc "Tools"
                waitAptgetInstall
                apt-get ${aptopt} install iotop gconf-editor vim
                ###################################################################################
                #desc "Programs"
                #waitAptgetInstall
                #apt-get ${aptopt} install okular gimp gimp-data-extras gimp-gutenprint  \
                #                   gimp-gmic gimp-plugin-registry gnome-rdp \
                #                   remmina remmina-plugin-gnome remmina-plugin-nx \
                #                   remmina-plugin-rdp remmina-plugin-vnc
                ###################################################################################
                #desc "E-mail"
                #waitAptgetInstall
                #apt-get ${aptopt} install thunderbird enigmail thunderbird-gnome-support \
                #                   xul-ext-calendar-timezones xul-ext-gdata-provider \
                #                   xul-ext-lightning
                ###################################################################################
                #desc "Editors"
                #waitAptgetInstall
                #apt-get ${aptopt} install kile kile-doc gv wv texlive-extra-utils \
                #                   lyx lyx-common menu dvipost latex2html latex2rtf \
                #                   tex4ht tth writer2latex hevea libtiff-tools vim \
                #                   chktex texlive doxygen texlive-base-bin emacs
                ###################################################################################
                desc "Programming"
                waitAptgetInstall
                apt-get ${aptopt} install build-essential default-jre
                #apt-get ${aptopt} install build-essential mpich2 gfortran cfortran default-jre \
                #                   default-jre-headless gromacs tkgate xfig xfig-doc
                ###################################################################################
                #desc "visualization"
                #waitAptgetInstall
                #apt-get ${aptopt} install grace
                ###################################################################################
                #desc "Chat"
                #waitAptgetInstall
                #apt-get ${aptopt} install pidgin pidgin-plugin-pack pidgin-sipe pidgin-themes \
                #                   pidgin-twitter pidgin-facebookchat pidgin-encryption \
                #                   pidgin-librvp pidgin-extprefs
                ###################################################################################
                #desc "WINE"
                #waitAptgetInstall
                #echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true | debconf-set-selections
                #apt-get ${aptopt} install wine1.4 wine1.4-amd64 winetricks q4wine gnome-exe-thumbnailer \
                #                   ttf-liberation ttf-mscorefonts-installer playonlinux
                ###################################################################################
                desc "Media"
                waitAptgetInstall
                apt-get ${aptopt} install libdvdread4
                /usr/share/doc/libdvdread4/install-css.sh
                waitAptgetInstall
                apt-get ${aptopt} install ubuntu-restricted-extras
                ###################################################################################
                desc "Backups"
                waitAptgetInstall
                apt-get ${aptopt} install luckybackup luckybackup-data backintime-gnome
                next
                reboot
                exit 0
                ;;
        4)      ###################################################################################
                desc "XFCE Shells"
                waitAptgetInstall
                add-apt-repository -y ppa:fossfreedom/xfwm4
                waitAptgetUpdate
                apt-get ${aptopt} update
                waitAptgetInstall
                apt-get ${aptopt} install xubuntu-desktop xfce4 xfce4-goodies xfwm4
                #########################################################################
                desc "Gnome Shells"
                #add-apt-repository -y ppa:gnome3-team/gnome3
                #add-apt-repository -y ppa:ricotz/testing
                #apt-get -y update
                #apt-get ${aptopt} install gnome gnome-shell gnome-session-fallback
                #apt-get ${aptopt} install gnome-shell-extentions-common
                waitAptgetInstall
                apt-get ${aptopt} install gnome-panel gnome-shell gnome-session-fallback gnome-tweak-tool
                ###################################################################################
                desc "Restore Splash Scheen"
                #rm                                                          /etc/alternatives/text.plymouth
                #ln -s /lib/plymouth/themes/ubuntu-text/ubuntu-text.plymouth /etc/alternatives/text.plymouth
                #rm                                                          /etc/alternatives/default.plymouth
                #ln -s /lib/plymouth/themes/ubuntu-logo/ubuntu-logo.plymouth /etc/alternatives/default.plymouth
                #update-initramfs -u
                echo FRAMEBUFFER=y | tee /etc/initramfs-tools/conf.d/splash
                waitAptgetInstall
                apt-get ${aptopt} remove plymouth-theme-xubuntu-text plymouth-theme-xubuntu-logo
                #apt-get ${aptopt} --reinstall install plymouth-theme-ubuntu-logo plymouth-theme-ubuntu-text
                next
                reboot
                exit 0
                ;;
        5)      desc "X2GO Server+Client ::: FreeNX client QTNX"
                waitAptgetUpdate
                add-apt-repository -y ppa:x2go/stable
                waitAptgetUpdate
                apt-get update
                waitAptgetInstall
                apt-get ${aptopt} install python-software-properties
                apt-cache search x2go
                #apt-get ${aptopt} install x2goserver x2goclient x2gognomebindings cups-x2go
                waitAptgetInstall
                apt-get ${aptopt} install x2goserver x2goclient cups-x2go
                echo
                waitAptgetUpdate
                sudo add-apt-repository ppa:freenx-team
                waitAptgetInstall
                apt-get ${aptopt} install qtnx
                ;;
        6)      desc 'Command line application launch # > grub-customizer'
                waitAptgetUpdate
                add-apt-repository -y ppa:danielrichter2007/grub-customizer
                waitAptgetUpdate
                apt-get update
                waitAptgetInstall
                apt-get ${aptopt} install grub-customizer
                ;;
        7)      desc 'Ubuntu Tweak and MyUnity'
                waitAptgetUpdate
                add-apt-repository -y ppa:tualatrix/ppa
                waitAptgetUpdate
                apt-get update
                waitAptgetInstall
                apt-get ${aptopt} install ubuntu-tweak myunity
                ;;
        8)      desc "Adobe, Java and Flash"
                echo acroread-common acroread-common/default-viewer select true | debconf-set-selections
                apt-get update
                add-apt-repository -y "deb http://archive.canonical.com/ $(lsb_release -sc) partner"
                waitAptgetUpdate
                sudo apt-get update
                waitAptgetInstall
                apt-get ${aptopt} install acroread
                waitAptgetInstall
                apt-get ${aptopt} install flashplugin-installer
                waitAptgetInstall
                apt-get ${aptopt} install flashplugin-downloader
                waitAptgetInstall
                apt-get ${aptopt} install flashplugin-nonfree-extrasound
                waitAptgetInstall
                apt-get ${aptopt} install adobe-flashplugin
                #add-apt-repository -y ppa:ferramroberto/java
                #apt-get update
                #apt-get ${aptopt} install sun-java6-jdk sun-java6-plugin
                waitAptgetInstall
                apt-get ${aptopt} install openjdk-6-jre
                waitAptgetInstall
                apt-get ${aptopt} install openjdk-7-jre
                ;;
        9)     desc "Plymouth Theme Manager"
                #apt-add-repository -y ppa:mefrio-g/plymouthmanager
                #apt-get ${aptopt} update
                #apt-get ${aptopt} install plymouth-theme* plymouth-manager
                ;;
        10)     desc "Setup Auto Updates"
                apt-get ${aptopt} install unattended-upgrades
                cat << EOF > /etc/apt/apt.conf.d/50unattended-upgrades
Unattended-Upgrade::Allowed-Origins {
        "\${distro_id} \${distro_codename}-security";
        "\${distro_id} \${distro_codename}-updates";
//      "\${distro_id} \${distro_codename}-proposed";
//      "\${distro_id} \${distro_codename}-backports";
};
EOF
                cat << EOF > /etc/apt/apt.conf.d/10periodic
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Download-Upgradeable-Packages "1";
APT::Periodic::AutocleanInterval "7";
APT::Periodic::Unattended-Upgrade "1";
EOF
                sed -i 's|^\(Prompt=\).*|\1never|' /etc/update-manager/release-upgrades
                #########################################################################
                echo "DONE: Review changes to the following files"
                echo -------------------------------------------
                echo "/etc/apt/apt.cond.d/50unattended-upgrades"
                echo "/etc/apt/apt.conf.d/10periodic"
                echo "/etc/update-manager/release-upgrades"
                ;;
        11)     desc "Setup Extras"
                #########################################################################
                waitAptgetUpdate
                add-apt-repository -y ppa:indicator-multiload/stable-daily
                waitAptgetUpdate
                add-apt-repository -y ppa:alexeftimie/ppa
                waitAptgetUpdate
                apt-get -y update
                waitAptgetInstall
                apt-get ${aptopt} install indicator-multiload indicator-sysmonitor
                #########################################################################
                mkdir /root/Downloads
                cd    /root/Downloads
                wget https://launchpad.net/~diesch/+archive/testing/+build/3076110/+files/classicmenu-indicator_0.07_all.deb
                waitAptgetInstall
                dpkg -i classicmenu-indicator_0.07_all.deb
                waitAptgetInstall
                apt-get ${aptopt} install python-gmenu
#               cat << EOF > /etc/xdg/autostart/classicmenu-indicator.desktop
#[Desktop Entry]
#Name=ClassicMenu Indicator
#Comment=Indicator applet to show the Gnome Classic main menu
#GenericName=ClassicMenu Indicator
#Categories=GNOME;Utility;
#Exec=classicmenu-indicator
#Icon=gnome-main-menu
#Type=Application
#EOF
                #########################################################################
                next
                reboot
                exit 0
                ;;
        *)      desc "Remove from rc.local"
                sed -i "/${scriptName}/s/^/#/" /etc/rc.local
                exit 0;;
esac

next
sleep 1
bash -l -c " \"$scriptPath/$scriptName\" >> \"/var/log/$scriptName\" " 2>&1 &






