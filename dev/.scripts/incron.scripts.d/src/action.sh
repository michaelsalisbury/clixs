#!/bin/bash
function main(){
	echo "$0" :: "$@" :: INCRON_ACTION :: ${INCRON_ACTION} :: INCRON_MATCH :: ${INCRON_MATCH}
}
. /etc/incron.scripts.d/src/scripts.common.sh
set_global_vars
main "$@" | tee -a "${LOG}"
