#!/bin/bash


fan='/sys/class/hwmon/hwmon2/device/pwm1'

echo 130 > "${fan}"
for delay in `seq 10`; do sleep 1; echo -n .; done
echo
for speed in `seq 130 255`; do
	echo ${speed} > "${fan}"
	for delay in `seq 2`; do sleep 0.75; echo -n .; done
	echo -n -e '\t'
	echo -n -e ${speed}'\t'
	sensors | awk '/fan1/{print $2}' 
done

