#!/bin/builder.sh
skip=( false false true false false true )
step=6
prefix="setup"

function setup_C(){
        desc 80 Beta C
        ###################################################################################
        uptime
}
function setup_D(){
        desc 80 Beta D
        ###################################################################################
        uptime
}
function setup_B(){
        desc 80 Beta B
	desc second message
	mesg 40 Something Nice
        ###################################################################################
        uptime
	return 1
}
function setup_E(){
        desc 80 Beta E
        ###################################################################################
        uptime
}
function setup_A(){
        desc 80 Beta A
        ###################################################################################
        uptime
}
