#!/bin/bash

# Updates
stageA=y
# Base software
#	vim emacs openssh-server putty pterm okular chromium-browser gnome-rdp remmina					\
#	g++ gfortran cfortran default-jre default-jre-headless								\
#	gimp gimp-data-extras gimp-gutenprint gimp-gmic gimp-plugin-registry						\
#	gromacs tkgate grace treb xfig xfig-doc										\
#	kile kile-doc gv texlive-extra-utils tex-guy									\
#	lyx lyx-common latex2html menu dvipost latex2rtf tex4ht wv tth writer2latex hevea libtiff-tools chktex		\
#	wine1.2 q4wine playonlinux ttf-liberation gnome-exe-thumbnailer							\
#	thunderbird xul-ext-mozgest enigmail xul-ext-flashgot thunderbird-gnome-support xul-ext-lightning latex-xft-fonts xul-ext-adblock-plus 
stage1=y
# X2GO VLC DVD OpenOffice Additions
stage2=y
# NAMD VMD ESPResSo 
stage3=
# Intel fortran/c/c++
stage4=y
# Skype
stage5=
stage6=

echo ------------------------------------------------------------------------------------------------------------------------
echo Skype
echo                       
[ -n "${stageA}" ] && {
apt-get -y update
apt-get -y upgrade
}

[ -n "${stage1}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo Software
echo                       
apt-get -y install													\
	vim emacs openssh-server putty pterm okular chromium-browser gnome-rdp remmina					\
	g++ gfortran cfortran default-jre default-jre-headless								\
	gimp gimp-data-extras gimp-gutenprint gimp-gmic gimp-plugin-registry						\
	gromacs tkgate grace treb xfig xfig-doc										\
	kile kile-doc gv texlive-extra-utils tex-guy									\
	lyx lyx-common latex2html menu dvipost latex2rtf tex4ht wv tth writer2latex hevea libtiff-tools chktex		\
	wine1.2 q4wine playonlinux ttf-liberation gnome-exe-thumbnailer							\
	thunderbird xul-ext-mozgest enigmail xul-ext-flashgot thunderbird-gnome-support xul-ext-lightning latex-xft-fonts xul-ext-adblock-plus 
}

[ -n "${stage2}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo VLC
echo
# VLC or other
wget http://www.medibuntu.org/sources.list.d/$(lsb_release -cs).list --output-document=/etc/apt/sources.list.d/medibuntu.list
apt-get -q update
apt-get --yes -q --allow-unauthenticated install medibuntu-keyring
apt-get -q update
apt-get -y install app-install-data-medibuntu apport-hooks-medibuntu
apt-get -y install w64codecs libdvdcss2
apt-get -y install vlc mozilla-plugin-vlc videolan-doc
}


[ -n "${stage2}" ] && {
# Open Office Additions
apt-get -y install			\
	openoffice.org			\
	ttf-mscorefonts-installer	\
	openoffice.org-writer2xhtml	\
	mozilla-openoffice.org		\
	gcj-jre				\
	gstreamer0.10-plugins-ugly	\
	gstreamer0.10-ffmpeg		\
	openoffice.org-writer2latex	\
	pstoedit			\
	gstreamer0.10-plugins-bad	
}

[ -n "${stage5}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo Skype
echo                       
# Skype
add-apt-repository 'deb http://download.skype.com/linux/repos/debian/ stable non-free'
apt-key adv --keyserver pgp.mit.edu --recv-keys 0xd66b746e 
aptitude update
apt-get -y install skype
}




[ -n "${stage2}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo X2GO
echo                       
# x2go
add-apt-repository 'deb http://x2go.obviously-nice.de/deb/ lenny main'
apt-key adv --keyserver keyserver.ubuntu.com --recv-key C509840B96F89133
apt-get update
apt-get -y install x2goserver-home x2gognomebindings x2goclient-gtk x2goclient x2goclient-cli
}

[ -n "${stage4}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo Intel Fortran & C/C++
echo                       
# intel fortran
wget http://registrationcenter-download.intel.com/akdlm/irc_nas/2135/l_fcompxe_2011.4.191.tgz
gunzip -c l_fcompxe_2011.4.191.tgz | tar -xvf -
echo \
"PSET_SERIAL_NUMBER=NR2M-299CWDV6
ACTIVATION=serial_number
CONTINUE_WITH_INSTALLDIR_OVERWRITE=yes
CONTINUE_WITH_OPTIONAL_ERROR=yes
PSET_INSTALL_DIR=/opt/intel/composerxe-2011.4.191
INSTALL_MODE=NONRPM
ACCEPT_EULA=accept
" > silent.fcomp
cd l_fcompxe_2011.4.191
./install.sh -s ../silent.fcomp
cd ~

# intel c/c++
wget http://registrationcenter-download.intel.com/akdlm/irc_nas/2136/l_ccompxe_2011.4.191.tgz
gunzip -c l_ccompxe_2011.4.191.tgz | tar -xvf -
echo \
"PSET_SERIAL_NUMBER=N5D5-DXCWHFW4
ACTIVATION=serial_number
CONTINUE_WITH_INSTALLDIR_OVERWRITE=yes
CONTINUE_WITH_OPTIONAL_ERROR=yes
PSET_INSTALL_DIR=/opt/intel/composerxe-2011.4.191
INSTALL_MODE=NONRPM
ACCEPT_EULA=accept
" > silent.ccomp
cd l_ccompxe_2011.4.191
./install.sh -s ../silent.ccomp
}


[ -n "${stage3}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo NAMD
echo                       
# namd

wget -O namd.tar.gz http://www.ks.uiuc.edu/Research/namd/2.8/download/794114/NAMD_2.8_Linux-x86_64-multicore.tar.gz
rm -rf			/opt/namd
mkdir			/opt/namd
tar -xvf namd.tar.gz -C /opt/namd
rm -f								/etc/profile.d/namd.sh
touch								/etc/profile.d/namd.sh
echo 'export PATH="'"$(cd /opt/namd/* && pwd)"':$PATH"' >	/etc/profile.d/namd.sh
cat								/etc/profile.d/namd.sh
echo /bin/bash							/etc/profile.d/namd.sh >	/etc/profile.d/namd.csh
cat												/etc/profile.d/namd.csh
}


[ -n "${stage3}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo VMD
echo                       
# vmd

wget -O vmd.tar.gz http://www.ks.uiuc.edu/Research/vmd/vmd-1.9/files/final/vmd-1.9.bin.LINUXAMD64.opengl.tar.gz
rm -rf                  /opt/vmd
mkdir                   /opt/vmd
tar -xvf vmd.tar.gz -C	/opt/vmd
cd			/opt/vmd/*
./configure LINUXAMD64 OPENGL FLTK TK NETCDF TCL PYTHON
cd			/opt/vmd/*/src
make install
}


[ -n "${stage3}" ] && {
echo ------------------------------------------------------------------------------------------------------------------------
echo ESPResSo
echo                       
# ESPResSo
apt-get -y install tcl8.5 tcl8.5-dev
wget -O espresso.tar.gz http://download.savannah.gnu.org/releases/espressomd/espresso-3.0.1.tar.gz
rm -rf                  	/opt/espresso
mkdir                   	/opt/espresso
tar -xvf espresso.tar.gz -C	/opt/espresso
cd				/opt/espresso/*
./configure
make
}



