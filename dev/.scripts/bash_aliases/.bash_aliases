alias apt-history='(cat /var/log/apt/history.log; zcat /var/log/apt/history.log.*.gz) | sed -n "/^Start-Date/{N;s/\n/ /;p}" | tac'
alias apt-history-install='apt-history | awk "/Commandline/&&/install/{gsub(/^|$/,\"@\",\$2);sub(/^/,\"@\",\$7);print}" | awk -F@ "{print \$2, \$4}"'
alias apt-history-remove='apt-history | awk "/Commandline/&&/remove/{gsub(/^|$/,\"@\",\$2);sub(/^/,\"@\",\$7);print}" | awk -F@ "{print \$2, \$4}"'
alias apt-history-purge='apt-history | awk "/Commandline/&&/remove/||/purge/{gsub(/^|$/,\"@\",\$2);sub(/^/,\"@\",\$7);print}" | awk -F@ "{print \$2, \$4}"'

alias my-zfs-utils="~/.scripts/my-zfs-utils.sh"
