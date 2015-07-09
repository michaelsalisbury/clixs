#!/bin/bash

function main(){
	usermod -c "${RANDOM}@sync-shadow" tcpdump
}
. /etc/incron.scripts.d/src/scripts.common.sh
set_global_vars
main "$@"
