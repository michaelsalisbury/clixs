#!/bin/bash

# ARD: enable with vnc
#
# 
# ruby: download gem - wget http://ruby-vnc.googlecode.com/files/ruby-vnc-1.1.0.gem
# ruby: install gem - gem install ruby-vnc-1.1.0.gem

# Auto Login User

username='net\mi164210'
password='X!tsanmyg77'
vncpasswd='5678'

username='localcosadmin'
password='COSTech2010!'


function login(){
	local login_username=${!1:-$1}
	local login_password=${!2:-$2}
	local login_vncpasswd=${!3:-$3}
	cat <<-RUBY | ruby -rubygems <(cat)
		require 'net/vnc'
		Net::VNC.open 'localhost:0', :shared => true, :password => '${login_vncpasswd}' do |vnc|
			vnc.type '${login_username}'
			vnc.key_press :tab
			vnc.type '${login_password}'
			vnc.key_press :return
		end
	RUBY
}


if ps -eo comm | grep -q SecurityAgent; then
	echo -n Waiting for process \"SecurityAgent\" to re-spawn.
	killall SecurityAgent &>/dev/null
	
	for i in `seq 10`; do
		if ps -eo comm | grep -q SecurityAgent; then
			echo
			sleep 1
			login username password vncpasswd
			exit 0
		fi
		sleep 1
		echo -n .
	done
	
	echo Process \"SecurityAgent\" not available.

else
	echo Process \"SecurityAgent\" not available, user must be logged in.
	echo Proceeding strait to login...
	login username password vncpasswd
fi



