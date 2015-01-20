#!/bin/bash

function test_me(){
	#echo hi
	echo what


}

function human_du(){
	du -cksh "${1}"/{.[a-zA-Z]*,*} 2>/dev/null
}
function sort_du(){
	du -cks "${1}"/{.[a-zA-Z]*,*} 2>/dev/null | sort -n
}

function main(){
	cat <<-AWK | awk -f <(cat) <(human_du "$@") <(echo CLEAR) <(sort_du "$@")
		#BEGIN { OFS="\t" }
		\$1 ~ "CLEAR|SORT" {
			_ = \$1
		}
		_ == "" {
			SIZE = \$1
			\$1 = ""
			#\$4 = ""
			\$0 = \$0
			\$1 = \$1
			print \$0
			ARY[\$0] = SIZE
			CNT += 1
		}
		_ == "CLEAR" {
			printf "\033["CNT"A"
			_ = "SORT"
			next
		}
		_ == "SORT" {
			\$1 = ""
			\$0 = \$0
			\$1 = \$1
			printf "\033[K"
			print ARY[\$0]"\t", \$0
		}
	AWK


















	return 
	cat <<-AWK | awk -f <(cat) <(du -cksh "${1}"/{.[a-zA-Z]*,*} 2>/dev/null)
		\$1 ~ "^[0-9\\\.]*$" {
			\$1 = \$1"B"
		}
		\$1 ~ "^[0-9]+[BGKM]$" {
			\$1 = gensub( /([0-9]+)([BGKM])/, "\\\1.0\\\2", "", \$1 )
		}
		{
			\$1 = gensub( /([0-9\\\.]+)([BGKM])/, "\\\1 \\\2", "", \$1 )	
			\$0 = \$0
			S = \$1
			M = \$2
			T = gensub( 
			

			print M, \$1, \$0
			CNT+=1
			ARY[CNT] = \$0
			#AR2[M""CNT] = \$0
		}
		END{
			print "DONE",CNT
			printf "\033["CNT"A"
			for (x = 1; x <= CNT; x++)
			{
				#printf "\033[K"
				#print "TADA", ARY[x]
			}

		}
	AWK


	#return 1



}
#function 


main "$@"
exit $?
