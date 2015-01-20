#!/bin/bash

wmctrl -l | grep -q "Outlook Web App" && {
	wmctrl -l | awk '/Outlook Web App/{print "wmctrl -i -a "$1}' | bash
} || {
	firefox -P apps -no-remote -new-window https://webmail.ucf.edu/ &
}
