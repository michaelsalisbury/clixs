#!/bin/bash
function command_history_filter(){
	exec 9<<-AWK
		BEGIN {
			split("$@",list)
			for (i in list) list[list[i]] = 0
		}
		{
			if (! (\$2 in list)) print
		}
	AWK
	awk -f <(cat <&9 9<&-) <(history)
} ; export -f command_history_filter

alias history-filter='command_history_filter'
alias apt-history='(cat /var/log/apt/history.log; zcat -q `ls -1 -t /var/log/apt/history.log.*.gz`) | sed -n "/^Start-Date/{N;s/\n/ /;p}" | tac'
alias apt-history-filter='awk "{gsub(/^|$/,\"@\",\$2);sub(/^/,\"@\",\$7);print}" | awk -F@ "{print \$2, \$4}"'
alias apt-history-install='apt-history | awk "/Commandline/&&/install/{print}"         | apt-history-filter'
alias apt-history-remove=' apt-history | awk "/Commandline/&&/remove/{print}"          | apt-history-filter'
alias apt-history-purge='  apt-history | awk "/Commandline/&&/remove/||/purge/{print}" | apt-history-filter'

alias my-zfs-utils="~/.scripts/my-zfs-utils.sh"
