#!/bin/bash

function main(){
	systemctl restart incrond.service
}
. /etc/incron.scripts.d/src/scripts.common.sh
set_global_vars
#main "$@" | tee -a "${LOG}"
main "$@"
