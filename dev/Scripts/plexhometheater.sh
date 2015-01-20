#!/bin/bash

export DISPLAY=:0.0
su - msalisbury <<< 'nohup plexhometheater.sh &>/dev/null' &
sleep 5
su - msalisbury <<< 'xdotool mousemove_relative 1 1'

