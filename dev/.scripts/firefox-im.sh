#!/bin/bash
. ${BASH_SOURCE%/*}/firefox-common.sh

function usage(){
	cat <<-USAGE
		usage:
		-h :: print these usage instructions
		-b :: backup before merge
		-d :: source default firefox deskop file
		-e :: open ini file with gedit then update
		-n :: no backup before merge (default)
		-u :: udate instead of sourcing default firefox desktop file (default)
	USAGE
}
function process_args(){
	INI="${HOME}/.mozilla/firefox/launchers.ini"
	TFD="${HOME}/.mozilla/firefox/firefox.desktop"
	DFD="/usr/share/applications/firefox.desktop"
	UFD="${HOME}/.local/share/applications/firefox.desktop"
	SOURCE_DEFAULT_ICON='false'
	BACKUP_BEFORE_MERGE='false'
	while getopts "h?bdenu" OPT; do
		case "${OPT}" in
			h|\?)	usage
					exit 0;;
			e)		gedit "${INI}";;	
			b)		BACKUP_BEFORE_MERGE='true';;
			d)		SOURCE_DEFAULT_ICON='true';;
			n)		BACKUP_BEFORE_MERGE='false';;
			u)		SOURCE_DEFAULT_ICON='false';;
		esac
	done
	return ${OPTIND}
}
function backup_firefox_desktop(){
	echo -n
}
function main(){
	process_args "$@"
	# determine source firefox.desktop file
	if ${SOURCE_DEFAULT_ICON} && [ -f "${DFD}" ]; then
		SFD="${DFD}"
	elif [ -f "${UFD}" ]; then
		SFD="${UFD}"
	elif ! [ -f "${DFD}" ]; then
		# throw error befause firefox is probably not installed
		echo -n
	else
		SOURCE_DEFAULT_ICON='true'
		SFD="${DFD}"
	fi

	# backup FTD if exist and requested
	if ${BACKUP_BEFORE_MERGE} && [ -f "${TFD}" ]; then
		backup_firefox_desktop
	fi

	# copy new firefox.desktop fle from default source if requested or missing
	if ${SOURCE_DEFAULT_ICON} && ! cp -v "${DFD}" "${UFD}"; then
		# fatal error exit
		echo -n
		exit 2
	fi

	# relink TFD, it's simpler then checking
	if ! ln -vf "${UFD}" "${TFD}"; then
		# fatal error exit
		echo -n
		exit 3
	fi

	# check for INI
	if ! [ -f "${INI}" ]; then
		# fatal error exit
		echo -n
		exit 4
	fi
	
	# merge
	merge_firefox_desktop

	# touch desktop shortcut file to force launcher to update
	touch "${UFD}"
	

}
function firefox_desktop_list_actions(){
	sed -n '/^Actions=/{s/^Actions=//;s/;/\n/g;s/\n$//p}' "${TFD}"
}
function INI_list_entries(){
	egrep -v "^[[:space:]]*(#|$)" "${INI}"
}
function INI_list_titles(){
	sed 's/[[:space:]]\+:[[:alpha:]].*//' <(INI_list_entries)
}
function INI_get_action_block_name(){
	tr -c [\\n[:alpha:]] _ 
}
function INI_get_entry(){
	local ACTION_TITLE=${1}
	cat <<-SED | sed -n -f <(cat) <(INI_list_entries)
		/^${ACTION_TITLE}[[:space:]]\+:[[:alpha:]]/{
			s/^${ACTION_TITLE}[[:space:]]\+:\([[:alpha:]]\)/\1/
			s/[[:space:]]\+/ /
			s/[[:space:]]\+/ /2
			/^disable[d]\?[[:space:]]/In	# skip entries who's profile is named disable(d)
			/^remove[d]?[[:space:]]/In		# skip entries who's profile is named remove(d)
			/^delete[d]?[[:space:]]/In		# skip entries who's profile is named delete(d)
			p
		}
	SED
}
function merge_firefox_desktop(){
	local ACTION_TITLE ACTION_BLOCK_NAME ACTION_OPTS
	while read ACTION_TITLE; do
		ACTION_BLOCK_NAME=$(INI_get_action_block_name <<< "${ACTION_TITLE}")
		ACTION_OPTS=$(INI_get_entry "${ACTION_TITLE}")

		# remove all ACTION BLOCK entries
		# VIM :/^\[Desktop Action Plex]/;/^\[Desktop Action/- g/.*/d

		

		# skip entries who's profiles are set to disabled/removed/deleted
		# Also skip entries that returned no "entry" (INI_get_entry)
		[ -z "${ACTION_OPTS}" ] && continue

		# Append ACTION BLOCK NAMES to end of Actions= line in desktop shortcut file
		cat <<-VIM | vim -T dumb --noplugin -n -es -S <(cat) "${TFD}"
			:/^Actions=/s/=/;/
			:/^Actions;/s/;${ACTION_BLOCK_NAME};/;/g
			:/^Actions;/s/$/${ACTION_BLOCK_NAME};/
			:/^Actions;/s/;/=/
			:wq
		VIM

		# add entries to end of file
		cat <<-DESKTOP_ACTION | sed '1i\\' >> "${TFD}"
			[Desktop Action ${ACTION_BLOCK_NAME}]
			Name=${ACTION_TITLE}
			Exec=bash -c "~/.scripts/firefox-pl.sh ${ACTION_OPTS}"
			OnlyShowIn=Unity;
		DESKTOP_ACTION
	done < <(INI_list_titles)
	
	# append Firefox Profile Manager entry
	ACTION_BLOCK_NAME="ProfileManager"
	cat <<-VIM | vim -T dumb --noplugin -n -es -S <(cat) "${TFD}"
		:/^Actions=/s/;${ACTION_BLOCK_NAME};/;/
		:/^Actions=/s/;${ACTION_BLOCK_NAME};/;/g
		:/^Actions=/s/$/${ACTION_BLOCK_NAME};/
		:wq
	VIM
	cat <<-DESKTOP_ACTION | sed '1i\\' >> "${TFD}"
		[Desktop Action ${ACTION_BLOCK_NAME}]
		Name=Firefox Profile Manager
		Exec=firefox -ProfileManager
		OnlyShowIn=Unity;
	DESKTOP_ACTION

	# append add new item entry
	ACTION_BLOCK_NAME="AddNewFirefoxLauncher"
	cat <<-VIM | vim -T dumb --noplugin -n -es -S <(cat) "${TFD}"
		:/^Actions=/s/;${ACTION_BLOCK_NAME};/;/
		:/^Actions=/s/;${ACTION_BLOCK_NAME};/;/g
		:/^Actions=/s/$/${ACTION_BLOCK_NAME};/
		:wq
	VIM
	cat <<-DESKTOP_ACTION | sed '1i\\' >> "${TFD}"
		[Desktop Action ${ACTION_BLOCK_NAME}]
		Name=Add New Firefox Launcher
		Exec=$(readlink -f "$0") -e
		OnlyShowIn=Unity;
	DESKTOP_ACTION



}
main "$@"

