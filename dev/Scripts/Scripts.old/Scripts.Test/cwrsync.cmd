@ECHO OFF
REM *****************************************************************
REM
REM CWRSYNC.CMD - Batch file template to start your rsync command (s).
REM
REM By Tevfik K. (http://itefix.no)
REM *****************************************************************

REM Make environment variable changes local to this batch file
SETLOCAL

REM ** CUSTOMIZE ** Specify where to find rsync and related files (C:\CWRSYNC)
SET CWRSYNCHOME=%PROGRAMFILES(x86)%\CWRSYNC

REM Set HOME variable to your windows home directory. That makes sure 
REM that ssh command creates known_hosts in a directory you have access.
SET HOME=%HOMEDRIVE%%HOMEPATH%

REM Make cwRsync home as a part of system PATH to find required DLLs
SET CWOLDPATH=%PATH%
SET PATH=%CWRSYNCHOME%\BIN;%PATH%

REM Windows paths may contain a colon (:) as a part of drive designation and 
REM backslashes (example c:\, g:\). However, in rsync syntax, a colon in a 
REM path means searching for a remote host. Solution: use absolute path 'a la unix', 
REM replace backslashes (\) with slashes (/) and put -/cygdrive/- in front of the 
REM drive letter:
REM 
REM Example : C:\WORK\* --> /cygdrive/c/work/*
REM 
REM Example 1 - rsync recursively to a unix server with an openssh server :
REM
REM       rsync -r /cygdrive/c/work/ remotehost:/home/user/work/
REM
REM Example 2 - Local rsync recursively 
REM
REM       rsync -r /cygdrive/c/work/ /cygdrive/d/work/doc/
REM
REM Example 3 - rsync to an rsync server recursively :
REM    (Double colons?? YES!!)
REM
REM       rsync -r /cygdrive/c/doc/ remotehost::module/doc
REM
REM Rsync is a very powerful tool. Please look at documentation for other options. 
REM

REM ** CUSTOMIZE ** Enter your rsync command(s) here

REM cd %CWOLDPATH%

	REM -vaxEhP

	REM a=rlptgoD
	REM r=recursive
	REM l=copy symlinks as symlinks
	REM t=copy times
	REM D=devices specials
	REM p=preserve permisions
	REM o=preserve owner
	REM g=preserve group
	REM E=preserve executablity
	REM x=don't cross filesystem boundaries
	REM h=human readable
	REM P=progress
	REM u=update

	REM --delete-delay=find deletions during, delete after (combine with -d and --backup-dir)
	REM -d --backup-dir=F:\Data.Changes


rsync -vrultxhPud --delete-delay --backup-dir=/cygdrive/f/Data.Changes  /cygdrive/c/users/msalisbury/Scripts/* localcosadmin@10.173.119.78:/home/localcosadmin/Scripts/Scripts.Test/

REM ssh-keygen

REM rsync --help

REM ssh --help
