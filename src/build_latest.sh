#!/bin/bash

ROOT=$(dirname "$0")
REPO="clixs"
LATEST=$(cat ${ROOT}/latest)
GIT="git add --all .; git commit -m $(basename "$0")_${REPO}_${LATEST}; git push"

if [ -f "${ROOT}/${REPO}_${LATEST}/DEBIAN/control" ]; then
	cat <<-SED | sed -i -f <(cat) "${ROOT}/${REPO}_${LATEST}/DEBIAN/control"
		/^Version: /cVersion: ${LATEST}
	SED
	cd "${ROOT}"
	dpkg-deb --build "${REPO}_${LATEST}"
	eval "${GIT}"
	echo
	echo "${GIT}"
else
	echo Missing \"${ROOT}/${REPO}_${LATEST}/DEBIAN/control\". 1>&2
	echo Exiting\! 1>&2
fi
