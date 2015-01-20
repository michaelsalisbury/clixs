#!/bin/bash

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
