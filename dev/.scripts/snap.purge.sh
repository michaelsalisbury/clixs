#!/usr/bin/bash

function main(){
	local KEEP="/tmp/snapshot.keep"
	local CUT="/tmp/snapshot.cut"
	echo -n > "${KEEP}"
	echo -n > "${CUT}"

	while read DATASET DAILY WEEKLY DATASET_FILTER; do
		purge "${DATASET}" "${DAILY}" "${WEEKLY}" "${DATASET_FILTER}"
	done <<-DATASETS
		rpool/ROOT/s10x_u11wos_24a        0:00 Sun auto-snap
		rpool/export/backups              4:00 Sun auto-snap
		rpool/export/workstations/backups 4:00 Sun auto-snap
		rpool/export/workstations/homes   0:00 Sun auto-snap
	DATASETS
		#rpool/ROOT/s10x_u11wos_24a        0:00 Sun auto-snap
		#rpool/export/backups              4:00 Sun auto-snap
		#rpool/export/workstations/backups 4:00 Sun auto-snap
		#rpool/export/workstations/homes   0:00 Sun auto-snap
}
function purge(){
	local DATASET=$1
	local DAILY=$2
	local WEEKLY=$3
	shift 3
	local DATASET_FILTER=$*

	local EPOCH_TABLE="/tmp/$$_snapshots_epoch-table"
	local DATE_TABLE="/tmp/$$_snapshots_date-table"
	local DATA_TABLE="/tmp/$$_snapshots_data-table"

	zfs get -r -Hp -o value type,name,creation "${DATASET}" |
		sed -n '/^snapshot$/ {n;h;n;H;x;s/\n/ /gp;}' > "${EPOCH_TABLE}"

	zfs list -r -H -o name,creation "${DATASET}" > "${DATE_TABLE}"

	join -j 1 "${EPOCH_TABLE}" "${DATE_TABLE}" |
		grep "${DATASET_FILTER}" > "${DATA_TABLE}"

	local sec_Hour=$(( 60 * 60 ))
	local sec_Day=$(( sec_Hour * 24 ))
	local sec_Week=$(( sec_Day * 7 ))
	local sec_Half=$(( sec_Week * 26 ))
	local sec_Year=$(( sec_Week * 52 ))
	local EPOCH=$(epoch)

	local one_week=$(( EPOCH - sec_Week ))
	local two_weeks=$(( EPOCH - sec_Week * 2 ))
	local four_weeks=$(( EPOCH - sec_Week * 4 ))
	local six_weeks=$(( EPOCH - sec_Week * 6 ))
	local eight_weeks=$(( EPOCH - sec_Week * 8 ))
	local half_year=$(( EPOCH - sec_Half ))
	local one_year=$(( EPOCH - sec_Year ))

	local SNAPSHOT STAMP CREATION DOW MON DAY TIME YEAR LAST
	while read SNAPSHOT STAMP CREATION; do
		read DOW MON DAY TIME YEAR <<< "${CREATION}"

		# keep hourly's until...
		if (( one_week < STAMP )); then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			continue
		fi
		
		if [ "${TIME}" != "${DAILY}" ]; then
			echo ${SNAPSHOT} :: ${CREATION} >> "${CUT}"
			zfs destroy "${SNAPSHOT}"
			continue
		fi
		
		# keep dayly's until...
		if (( four_weeks < STAMP )); then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			continue
		fi

		if [ "${DOW}" != "${WEEKLY}" ]; then
			echo ${SNAPSHOT} :: ${CREATION} >> "${CUT}"
			zfs destroy "${SNAPSHOT}"
			continue
		fi

		# keep weekly's until...
		if (( half_year < STAMP )); then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			LAST=${MON}
			continue
		fi
		
		# strip out all but the first snap of each month
		if [ "${LAST}" == "${MON}" ]; then
			echo ${SNAPSHOT} :: ${CREATION} >> "${CUT}"
			zfs destroy "${SNAPSHOT}"
			LAST=${MON}
			continue
		else
			LAST=${MON}
		fi
	
		# keep monthly's untill...
		if (( one_year < STAMP )); then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			continue
		fi

		# keep only January and July of previos years
		if [ "${MON}" == "Jan" ]; then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			continue
		elif [ "${MON}" == "Jul" ]; then
			echo ${SNAPSHOT} :: ${CREATION} >> "${KEEP}"
			continue
		else
			echo ${SNAPSHOT} :: ${CREATION} >> "${CUT}"
			zfs destroy "${SNAPSHOT}"
		fi
	done < "${DATA_TABLE}"
	
	rm -f -- "/tmp/$$_"*

}
function epoch(){
	perl -e 'print time(), "\n"'
}
main "$@"
