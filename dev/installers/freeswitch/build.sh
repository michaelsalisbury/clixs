#!/bin/bash

VERSION='v1.6'
BASE="/opt/freeswitch"
RSRC="/opt/freeswitch/resources"
GIT="${BASE}/${VERSION}"
CPUS='2'


mkdir -p "${GIT}"
mkdir -p "${RSRC}"
mkdir -p "${RSRC}/libyuv"

#git clone -b ${VERSION} https://freeswitch.org/stash/scm/fs/freeswitch.git "${GIT}"
#git clone https://mikej@freeswitch.org/stash/scm/sd/libyuv.git "${RSRC}/libyuv"

false && {
#true && {
	for URL in \
		'https://mirror.umd.edu/ubuntu/pool/main/libv/libvpx/libvpx2_1.4.0-4_amd64.deb' \
		'https://mirror.umd.edu/ubuntu/pool/main/libv/libvpx/libvpx-dev_1.4.0-4_amd64.deb'
	do
		rm   -f "${RSRC}/$(basename "${URL}")" 
		wget -O "${RSRC}/$(basename "${URL}")" "${URL}"
		dpkg -i "${RSRC}/$(basename "${URL}")"
	done		

	apt-get install -y -f
}

false && {
#true && {
	add-apt-repository ppa:freeswitch-drivers/freeswitch-nightly-drivers
	apt-get update
	apt-get install build-essential flex bison libncurses5-dev unixodbc-dev libnspr4 autoconf automake libtool libltdl3-dev
	apt-get install python-software-properties
	apt-get update
	apt-get install -y devscripts gawk g++ git-core libjpeg-dev make python-dev pkg-config libtiff4-dev libperl-dev libgdbm-dev libdb-dev gettext equivs mlocate git dpkg-dev wget sox flac
	apt-get install -y -f
	apt-get install -y sqlite3 libsqlite3-dev
	apt-get install libcurl3 libcurl4-gnutls-dev
	apt-get install -y libpcre3-dev speex libspeex-dev libspeexdsp-dev libldns-dev libedit-dev clang libvpx-dev libvpx1 libvpx-dev
	apt-get install -y lua5.2 liblua5.2-dev libopus-dev libsndfile1-dev
	apt-get install -y -f
	update-alternatives --set awk /usr/bin/gawk
}

false && {
#true && {
	cd "${RSRC}/libyuv"
	make -j ${CPUS}
	make install

}

false && {
#true && {
	cd "${GIT}"
	./bootstrap.sh
	./configure
	make -j ${CPUS}
	make install
}

exit
