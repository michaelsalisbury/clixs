#!/bin/bash

opts='-vaxEAHXPu'

rsync ${opts}						\
	--delete-during					\
	--max-size=1000.0m				\
	--exclude=MatLab				\
	--exclude=SW_DVD5_Office_Mac*	\
	G88192RMXYK:/var/www/packages/	/var/www/packages/
	#--include=ISO.Linux**	\
	#--exclude=**			\


exit
	--exclude=Apps_Linux/CrossOverOffice2010.tgz	\
	--exclude=Apps_Linux/MatLab*					\
	--exclude=Apps_Linux/Mathematica*				\

rsync -vaxEAHXPhu		\
	--delete-during		\
	--include=/etc**	\
	--include=/bin**	\
	--include=/boot**	\
	--include=/lib**	\
	--include=/opt**	\
	--include=/sbin**	\
	--include=/usr**	\
	--include=/srv**	\
	--include=/var**	\
	--include=/selinux**	\
	--include=/tmp		\
	--include=/run**	\
	--include=/root**	\
	--include=/lost+found**	\
	--exclude=**		\
	/ /mnt/sdd1/root.bk2014May17/
