#!/bin/bash

echo $@
echo ${*:2:2}
echo ${*:$#}
echo ${*:0:$#-1}

