#!/bin/bash


function main(){
	whoami | grep -q ^root$ || exit 1

	cat <<-AWK |
		BEGIN{
			CMD_PPID="ps --no-heading -o ppid -p "
		}
		{
			PORT=\$5
			PROCESS=\$10
			PID=\$11
			UID=\$14
		}
		PROCESS != "sshd" {next}
		UID     != "1000" {next}
		{
			(CMD_PPID PID) | getline PPID
			print PORT,PID,PPID
		}
	AWK
      	awk -F "[ :\",]+" -f <(cat) < <(ss -npelt4)|
	while read trgPORT trgPID trgPPID; do
		cat <<-AWK |
			function hostname(PORT)
			{
				SSH="ssh -n -p " PORT SSH_OPTS " localhost hostname"
				SSH | getline NAME
				return NAME
			}
			BEGIN{
				trgPORT="${trgPORT}"
				trgPID="${trgPID}"
				trgPPID="${trgPPID}"
				SSH_OPT["StrictHostKeychecking"]="no"
				SSH_OPT["PasswordAuthentication"]="no"
				#SSH_OPT["RSAAuthentication"]="yes"
				SSH_OPT["PubkeyAuthentication"]="yes"
				SSH_OPT["ConnectionAttempts"]=1
				SSH_OPT["ConnectTimeout"]=1
				for (i in SSH_OPT) SSH_OPTS=SSH_OPTS " -o " i "=" SSH_OPT[i]
			}
			{
				PPID=\$19
				srcPID=\$15
				srcIP=\$6
			}
			PPID != trgPPID {next}
			{
				print trgPORT, trgPID, trgPPID, srcPID, srcIP, hostname(trgPORT)
			}
		AWK
      		awk -F "[ :\",]+" -f <(cat) < <(ss -npeat4) &

	



		#echo $PORT

	done|
	column -t
      	#awk -F "[ :\",]+"  -f <(cat) < <(ss -npelt4)

	return
	local NETSTAT='netstat -tunpae'
	cat <<-AWK |
		BEGIN{
			NETSTAT="netstat -tunpae"
			SSH_OPT["StrictHostKeychecking"]="no"
			SSH_OPT["PasswordAuthentication"]="no"
			#SSH_OPT["RSAAuthentication"]="yes"
			SSH_OPT["PubkeyAuthentication"]="yes"
			SSH_OPT["ConnectionAttempts"]=1
			SSH_OPT["ConnectTimeout"]=1
			for (i in SSH_OPT) SSH_OPTS=SSH_OPTS " -o " i "=" SSH_OPT[i]
		}
		\$12!="sshd"{next}
		\$9!="1000"{next}
		{
			PORT=\$5
			PID=\$11
			PPID="ps --no-heading -o ppid -p " PID
			PPID | getline PPID
			SOC=NETSTAT " | awk '/" PPID "/{print \$5, \$9}'"
			SOC | getline SOC
			SSH="ssh -n -p " PORT SSH_OPTS " localhost hostname"
			SSH | getline SSH_OUT
			
			print PORT, PID, PPID, SOC, SSH_OUT
		}
		

	AWK
      	awk -F "[ :/]+"  -f <(cat) < <(${NETSTAT}) |
	column -t
}
main "$@"
