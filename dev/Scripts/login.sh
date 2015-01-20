#!/bin/bash
echo hi

# defaults write /Library/Preferences/com.apple.loginwindow SHOWFULLNAME -bool true
# killall loginwindow
# defaults write /Library/Preferences/.GlobalPreferences MultipleSessionEnabled -bool true
# opensnoop -n cfprefsd
# apt-get install xtightvncviewer
# ruby: download gem - wget http://ruby-vnc.googlecode.com/files/ruby-vnc-1.1.0.gem
# ruby: install gem - gem install ruby-vnc-1.1.0.gem

# Auto Login User

cat <<-RUBY | ruby -rubygems <(cat)
	require 'net/vnc'
	Net::VNC.open 'localhost:0', :shared => true, :password => '5678' do |vnc|
		vnc.type 'localcosadmin'
		vnc.key_press :tab
		vnc.type 'COSTech2010!'
		vnc.key_press :return
	end
RUBY

sleep 2
who

exit 0

# remote suspend to fast user switch

cat <<-SUSPEND > /tmp/suspend.bash.sh
	sleep 3
	"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" -suspend
SUSPEND

cat <<-SU | su - localcosadmin
	osascript <<-APPLESCRIPT
		tell application "System Events"
			-- Command is 55, shift is 56, caps lock 57, option is 58, control is 59. 
			-- escape key
			key code 53
			key code 53
			delay 1
			-- space bar
			key code 49 using command down
			delay 1
			-- type
			keystroke "Terminal"
			delay 1
			keystroke return
			delay 1
			keystroke "nohup /bin/bash /tmp/suspend.bash.sh &"
			keystroke return
			delay 1
			-- exit command line
			keystroke "exit 0"
			keystroke return
			delay 1
			-- quit window
			keystroke "q" using command down
		end tell
	APPLESCRIPT
SU

rm /tmp/suspend.bash.sh


exit 0


# setup remote ssh access
# setup firewall rules
systemsetup -setremotelogin on

# setting up remote desktop :: enable

launchctl load -w /System/Library/LaunchDaemons/com.apple.screensharing.plist
defaults write /Library/Preferences/com.apple.RemoteManagement ScreenSharingReqPermEnabled -bool false
defaults write /Library/Preferences/com.apple.RemoteManagement VNCLegacyConnectionsEnabled -bool true
/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -configure -clientopts -setvncpw -vncpw 5678

# setting up remote desktop :: disable
#sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.screensharing.plist
#defaults write /Library/Preferences/com.apple.RemoteManagement ScreenSharingReqPermEnabled -bool false
#defaults write /Library/Preferences/com.apple.RemoteManagement VNCLegacyConnectionsEnabled -bool false


# setting up remote desktop :: old way don't use

# Activate Remote Desktop Sharing, enable access privileges for all users, restart ARD Agent:
#/usr/libexec/ApplicationFirewall/socketfilterfw --add /System/Library/CoreServices/RemoteManagement/screensharingd.bundle/Contents/MacOS/screensharingd
#/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -activate -configure -access -on -restart -agent -privs -all 

# If you just want to stop the ARD Agent process:
#/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -agent -stop

# If you want to deactivate it:
#/usr/libexec/ApplicationFirewall/socketfilterfw --remove /System/Library/CoreServices/RemoteManagement/screensharingd.bundle/Contents/MacOS/screensharingd
#/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -deactivate -configure -access -off

# For more information about using the kickstart command, add the -help flag. For example:
#/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -help


exit 0



cat <<-SU | su - localcosadmin
	osascript <<-APPLESCRIPT
		tell application "System Events"
			-- Command is 55, shift is 56, caps lock 57, option is 58, control is 59. 
			-- escape key
			key code 53
			key code 53
			delay 1
			-- space bar
			key code 49 using command down
			delay 1
			-- type
			keystroke "Terminal"
			delay 1
			keystroke return
			delay 1
			keystroke "nohup { sleep 4; \"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession\" -suspend; } &"
			keystroke return
			delay 1
			keystroke "exit 0"
			keystroke return
			delay
			key code 07 using command down
		end tell
		
		
		--tell application "System Events"
			--tell process "Finder" to keystroke "t" using command down
        	--end tell
		--tell application "Terminal"
			--activate
			--do script "\"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession\" -suspend"
			--quit
		--end tell
	APPLESCRIPT
SU


exit 0


cat <<-SU | su - localcosadmin
	osascript <<-APPLESCRIPT
		tell application "System Events" to key code 53
		tell application "System Events" to key code 53
		delay 1
		tell application "System Events" to key code 49 using command down
		delay 1
		tell application "System Events" to keystroke "Terminal"
		tell application "System Events" to keystroke return
		
		
		--tell application "System Events"
			--tell process "Finder" to keystroke "t" using command down
        	--end tell
		--tell application "Terminal"
			--activate
			--do script "\"/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession\" -suspend"
			--quit
		--end tell
	APPLESCRIPT
SU


exit 0


osascript <<-APPLESCRIPT
	tell application "Finder" to close every window
	tell application "Finder" to quit
	delay 1
	tell application "Finder"
		open home
		open location "cifs://mi164210@net1110.net.ucf.edu/profiles/mi164210"
	end tell
	delay 1
	tell application "System Events"
		tell process "Finder" to keystroke "t" using command down
	end tell
APPLESCRIPT



#exit 0

osascript <<-APPLESCRIPT
	delay 1
	tell application "Finder" to close every window
	tell application "Finder" to quit
APPLESCRIPT

sleep 1
		
while read key value; do
	defaults write com.apple.finder ${key} "${value}"
done <<-KEYS
	ShowExternalHardDrivesOnDesktop 1
	ShowHardDrivesOnDesktop		1
	ShowMountedServersOnDesktop	1
	ShowRemovableMediaOnDesktop	1
	NewWindowTarget			PfCm
	NewWindowTargetPath
KEYS

sleep 1

osascript <<-APPLESCRIPT
	tell application "Finder" to activate
	tell application "Finder" to open home
	activate application "Finder" --gives Finder focus
APPLESCRIPT

exit 0


KeyAppleScript key code
esc53
F1122
F2120
F399
F4118
F596
F697
F798
F8100
F9101
F10109
F11103
tab48?
`50
118
219
320
421
523
622
726
828
925
029
[27
]24
delete51
'12
,13
.14
p15
y17
f16
g32
c34
r31
l35
/33
=30
\42
a0
o1
e2
u3
i5
d4
h38
t40
n37
s41
-39
return36
;6
q7
j8
k9
x11
b45
m46
w43
v47
z44
space49
enter52
left123
up126
down125
right124


