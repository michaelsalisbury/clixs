#!/bin/bash

function json_object_to_vars(){
	local json_var_prefix=${2:+${1}}
	local json_query=${2:-${1}}
	while read line; do
		eval ${json_var_prefix}${line}
		eval ${json_var_prefix}${FUNCNAME}+=\${json_var_prefix}\${line%%=*}\\\ 
	done < <(jq -r -M "${json_query}" | sed '1d;$d;s/": /=/;s/,$//;s/^ *"//')
		eval ${json_var_prefix}${FUNCNAME}+=\${json_var_prefix}\${FUNCNAME}
}
function sync_lock(){
	local lock_path="${WORK}/${APP}.lock"
	local lock_path_update_request="${WORK}/${APP}.rerun"
	sync_lock_ ${1}
}
function main(){
	if sync_lock -t; then
		sync_lock -r
		sync_lock -e
	else
		sync_lock -u
		return 2
	fi
	# verify freenas config
	for VAR in SYSTEM API_USER API_PASS API_URL_PREFIX HTTP_METHOD JSON OAUTH; do
		VAL=${!VAR}
		if ! (( ${#VAL} )); then
			echo Sorry freenas config variable \"${VAR}\" is not set. Exiting\!
			return 1
		fi
	done

	# verify freenas acction variables unique to this script have been set
	for VAR in min_account_id max_account_id freenas_user_home_root freenas_user_shell; do
		VAL=${!VAR}
		if ! (( ${#VAL} )); then
			echo Sorry freenas config variable \"${VAR}\" is not set. Exiting\!
			return 1
		fi
	done

	# aquire account list from freenas
	accounts=$(API_ GET "account/users/"  2>/dev/null)
	  groups=$(API_ GET "account/groups/" 2>/dev/null)

	# parse local accounts and compare	bsdusr_username
	while read USR_NAME USR_ID USR_GID USR_HOME USR_COM ; do
		# split linux passwd user info field #5 by comma
		local IFS=$','
		read USR_INFO_FULLNAME USR_INFO_ROOM USR_INFO_WORK USR_INFO_HOME USR_INFO_EMAIL <<< "${USR_COM}"
		unset IFS

		# parse the freenas account list for a user with specific UID
		unset ${json_object_to_vars}
		json_object_to_vars ".[]|select(.bsdgrp_gid==${USR_ID})|." <<< "${groups}"
		bsdgrp_id=${id}
		json_object_to_vars ".[]|select(.bsdusr_uid==${USR_ID})|." <<< "${accounts}"

		# if ID exist in freenas then verify/fix username, full name, e-mail
		if (( id )); then
			while (( USR_ATTRIBUTE_TEST_CNT++ < 10 )); do
				# do usernames match
				unset DATA
				if [ "${USR_NAME}" != "${bsdusr_username}" ]; then
					echo deleting user \"${bsdusr_username}\"
					echo deleting group\"${bsdgrp_group}\" 
					API_ DELETE "account/users/${id}/" | jq .
					API_ DELETE "account/groups/${bsdgrp_id}/" | jq .
					sync_lock -u
					break
				fi

				if [ "${USR_INFO_FULLNAME:-${USR_NAME}}" != "${bsdusr_full_name}" ]; then
					echo fixing fullname for \"${USR_NAME}\" :: \"${USR_INFO_FULLNAME:-${USR_NAME}}\"
					bsdusr_full_name=${USR_INFO_FULLNAME:-${USR_NAME}}
					DATA[${#DATA[*]}]='bsdusr_full_name'
					#API_ PUT "account/users/${id}/" bsdusr_full_name | jq .
					#sync_lock -u
				fi
				if [ "${USR_INFO_EMAIL}" != "${bsdusr_email}" ]; then
					echo fixing e-mail for \"${USR_NAME}\" :: \"${USR_INFO_EMAIL}\"
					bsdusr_email=${USR_INFO_EMAIL}
					DATA[${#DATA[*]}]='bsdusr_email'
					#API_ PUT "account/users/${id}/" bsdusr_email | jq .
					#sync_lock -u
				fi
				if [ "${freenas_user_shell}" != "${bsdusr_shell}" ]; then
					echo fixing shell for \"${USR_NAME}\" :: \"${freenas_user_shell}\"
					bsdusr_shell=${freenas_user_shell}			
					DATA[${#DATA[*]}]='bsdusr_email'
					#API_ PUT "account/users/${id}/" bsdusr_shell | jq .
					#sync_lock -u
				fi
				if (( ${#DATA[*]} )); then
					echo submitting fix to freenas
					json_from_vars DATA | jq .
					API_ PUT "account/users/${id}/" DATA | jq .
					sync_lock -u
				else
					echo user \"${USR_NAME}\" is in sync.
					break
				fi
			done
		else
		# create user on freenas
			echo create user ${USR_NAME}
			# START_OBJECT_02
			bsdusr_username="${USR_NAME}"
			bsdusr_home="${freenas_user_home_root}/${USR_NAME}"
			bsdusr_uid="${USR_ID}"
			bsdusr_email="${USR_INFO_EMAIL}"
			bsdusr_password="${RANDOM}XYZ${RANDOM}"
			bsdusr_full_name="${USR_INFO_FULLNAME:-${USR_NAME}}"
			bsdusr_creategroup="true"
			bsdusr_shell="${freenas_user_shell}"
			object_from_preceeding_vars DATA START_OBJECT_02
			json_from_vars DATA | jq .
			API_ POST "account/users/" DATA | jq .
			chmod 700 "${USR_HOME}"
			rsync -vaxEAHPhu "$(sed -n 's/^SKEL=//p' /etc/default/useradd)/" "${USR_HOME}/"
			chown ${USR_ID}:${USR_GID} -R "${USR_HOME}"
			sync_lock -u
		fi
		
		# unset vars for next loop
		unset ${json_object_to_vars}
		unset ${!USR_*}
		unset USR_ATTRIBUTE_TEST_CNT bsdgrp_id
	done < <(cat <<-AWK | awk -F: -f <(cat) /etc/passwd
			{
				USR=\$1
				UID=\$3
				GID=\$4
				COM=\$5
				HOM=\$6
			}
			# list only user within the min/max account id range
			# display only the user name, user id and user info sections
			UID>=${min_account_id} && UID<=${max_account_id} {
				print USR, UID, GID, HOM, COM
			}
		AWK
		)
	# unlock and rerun if need be
	sync_lock -r || main
}
function API_(){
	# if arg #1 or arg #2 is unset we should pobably throw an error
	# arg #1 is a curl action GET|DELETE|POST
	# arg #2 is a url entry 
	# arg #3 sould be a json string or variable reference; POST action only otherwise ignored
	local API_X=${1^^}
	local API_URL=${2}
	GET_REF API_URL
	#local OAUTH="Authorization: Bearer $TOKEN"
	local OAUTH="Authorization: Basic "$(printf "${API_USER}:${API_PASS}" | base64)
	local JSON="Content-Type: application/json"
	local API_URL="${HTTP_METHOD}://${API_URL_PREFIX}/${API_URL}"
	case "${API_X}" in
		POST|PUT)
			shift 2
			# test arg #1 checking for a variable reference; if true build json from vars
			if IS_REF "$1"; then
				if [ "${1^^}" == "DATA" ]; then
					local API_DATA=`json_from_vars ${@} | jq -c -M .${1}`
				else
					local API_DATA=`json_from_vars ${@}`
				fi
			elif ! (( ${#@} )); then
				echo POST requires json data, API_DATA was empty. 1>&2
				return 1
			else
				local API_DATA="$@"
			fi
			curl -k -X ${API_X} -H "${JSON}" -H "${OAUTH}" -d "${API_DATA}" "${API_URL}" 
			;;
		GET)
			curl -k -X ${API_X} -H "${JSON}" -H "${OAUTH}" "${API_URL}"
			echo "${API_URL}" 1>&2
			;;
		DELETE)
			curl -k -X ${API_X} -H "${OAUTH}" "${API_URL}"
			echo "${API_URL}" 1>&2
			;;
		*)
			echo Unknown curl arg \"-X ${DO_API_X}\"
			return 2
			;;
	esac
}
function GET_REF(){
	local VAR=$1
	local REF=${2-${!VAR}}
	eval unset ${VAR}
	if IS_REF "${REF}"; then
		# treat all references as arrays
		eval ${VAR}=\( \"\${${REF}[@]}\" \)
	else
		# reference must be data
		eval ${VAR}=\"\${REF}\"
	fi
}
function IS_REF(){
	local REF=$1
	# test 1: are their any special characters
	# test 2: is first character a digit
	# test 3: is reference set
	! [[ "${REF//[^[:alpha:]_[:digit:]]/@}" =~ @ ]] &&
	[[ "${REF:0:1}" =~ [[:alpha:]_] ]] &&
	(( ${!REF+1} ))
}
function object_from_preceeding_vars(){
	# ARG #1 the variable name of the array to generate
	# ARG #2 the start line of the variables to place inside the array
	local VAR
	local OBJECT=${1}
	local START_VARS=${2}
	eval unset "${OBJECT}"

	while read VAR; do
		eval "${OBJECT}[\${#${OBJECT}[*]}]="\${VAR}
	done < <(	
		sed -n "/^[[:space:]#]*${START_VARS}\$/,$((BASH_LINENO-1)) p" "${0}"	|
		tac |
		sed -n "1,/^[[:space:]#]*${START_VARS}\$/{s/^[[:space:]]*\([a-zA-Z_][a-zA-Z_0-9]*\)=.*/\1/p}" |
		tac
	)
}
function json_from_vars(){
	local i
	local VAR=$1
	local IFS=$' \t\n'
	shift
	# if multiple variables were presented join them into a single json object
	if (( ${#@} )); then
		jq -M -c -s '.[0] + .[1]' <(${FUNCNAME} ${VAR}) <(${FUNCNAME} "$@")
		return	
	fi
	# get the contents of the variable VAR
	eval local  VAL=( \"\${${VAR}[@]}\" )
	# if the contents of VAR (VAL) are a list of variables then treat VAR as an object
	while true; do
		for i in `seq 0 $(( ${#VAL[*]} - 1 ))`; do
			# test if contents contain special characters that would disqualify them as variables
			if [[ "${VAL[$i]//[^[:alpha:]_[:digit:]]/@}" =~ @ ]]; then
				break 2
			# test if contents are a set variable
			elif ! (( ${!VAL[$i]+1} )); then
				break 2
			fi
		done
		# recusivelly create the object and then nest it under the parent
		${FUNCNAME} ${VAL[@]} | jq -s -M -c ".[] | { ${VAR}: . }"
		return
	done
	# iterate threw each index
	for i in `seq 0 $(( ${#VAL[*]} - 1 ))`; do
		# escape escapes
		VAL[$i]=${VAL[$i]//\\/\\\\}
		# escape double quotes
		VAL[$i]=${VAL[$i]//\"/\\\"}
		# don't enclose numbers in double quotes
		# don't enclose null|true|false in double quotes
		value_is_number "${VAL[$i]}" ||
		grep -q '^\(true\|false\|null\)$' <<< "${VAL[$i]}" ||
		VAL[$i]=\"${VAL[$i]}\"
		# don't append comma to last index
		(( ${#VAL[*]} - 1 > i ))     && VAL[$i]=${VAL[$i]},
	done
	# generate JSON
	if (( ${#VAL[*]} - 1 )); then
		# if bash var was array
		cat <<-JQ | jq -n -M -c "`cat`"
			{
				${VAR}: [ ${VAL[@]//\":ARRAY:\"*/} ]
			}
		JQ
	else
		# if bash var was string
		cat <<-JQ | jq -n -M -c "`cat`"
			{
				${VAR}: ${VAL}
			}
		JQ
	fi
}
function value_is_number(){
	(( ${#1} )) || return 1
	#local value=${1//[[:digit:].]/} # this strips out any characters that are not digits or decimal points
	echo ${1//[\$\`]/} |
	egrep '(^[[:digit:]]+$)|(^[[:digit:]]*[.][[:digit:]]*$)' |
	egrep -q -v '^[.]$'
	# the first egrep confirms that the value is formated as a number with or without a single decimal place
	# the second egrep command eliminates the posibility of a value with only a decimal place (a dot)
}
. /etc/incron.scripts.d/src/scripts.common.sh
set_global_vars
. "${WORK}/${APP}.cnf"		&>/dev/null ||
. "${WORK}/${APP%%[_-]}.cnf"	&>/dev/null ||
. "${WORK}/freenas.cnf"		&>/dev/null ||
{
	echo Failed to find freenas config info. Exiting\!
	exit 1
}
# lock time out
LOCK_TIMEOUT="120"

main "$@" | tee -a "${LOG}"
