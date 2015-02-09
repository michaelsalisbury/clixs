#!/bin/bash

exec 3<<-AWK
	NR==1,/^VBoxManage/{next}
	!RLENGTH{match(\$0,/^ */)}
	{print substr(\$0,RLENGTH+1)}
AWK
exec 4<<-SED
	1!{s/^[^ ]/!&/}		# excluding the first line; prepend ! to each line that doesn't start with a space
	s/^ \+/ /		# all lines; reduce all leading spaces to one space
	H			# all lines; append line to hold space
	\${			# last line;
		g		# copy hold space to pattern space
		s/\n//g		# remove all line line feeds
		s/!/\n/g	# replace ! with line feeds; this effectivelly adds indented lines to parrent
		s/<1-N>/1/g	# replace; self explanitory
		p
	}
SED
exec 5<<-SED
	/^[^ ]\+|\$/{		# match all lines that have a list of actions but no sub arguments
		s/|\$//		# remove trailing pipe/or
		s/|/\n/g	# replave interleaved pipes/ors with a line feed
	}
SED
vboxmanage controlvm|
awk    -f <(cat<&3) |
sed -n -f <(cat<&4) |
sed    -f <(cat<&5)

