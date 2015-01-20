#!/bin/bash
echo hi

# ARD: enable with vnc
#
# 
# ruby: download gem - wget http://ruby-vnc.googlecode.com/files/ruby-vnc-1.1.0.gem
# ruby: install gem - gem install ruby-vnc-1.1.0.gem

# Auto Login User

username=
password=
vncpasswd=


killall SecurityAgent

for i in `seq 10`; do
	if ps -eo comm | grep -q SecurityAgent; then
		break
	fi
	sleep 1
done


cat <<-RUBY | ruby -rubygems <(cat)
	require 'net/vnc'
	Net::VNC.open 'localhost:0', :shared => true, :password => '${vncpasswd}' do |vnc|
		vnc.type '${username}'
		vnc.key_press :tab
		vnc.type '${password}'
		vnc.key_press :return
	end
RUBY



