#!builder.sh
skip=( true false false false true true true true true true false )
step=2
prefix="setup"
#aptopt="-y -q --force-yes"
#autoLoginUser="msalisbury"

function setup_A(){
        desc 80 "A" B C
        ###################################################################################
        echo A
        uptime
}
function setup_B(){
        desc 80 Run Beta Now
        ###################################################################################
	./beta.sh
}
function setup_C(){
        desc 80 "C"
        ###################################################################################
        show_function $step
        uptime
        #echo C
}


