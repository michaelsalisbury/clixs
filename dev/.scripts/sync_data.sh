#!/bin/bash


src=G88192RMXYK:/data/
dst=/data/

rsync -vaEhPu \
	--delete-before\
	--exclude=Backups\
	--exclude=Drivers\
	--exclude=ISO.Mac\
	"${src}" "${dst}"




