#!/bin/bash
SOURCE='/mnt/sdc3-UBUNTU_DATA0/'
MIRROR='/mnt/sdd3-UBUNTU_DATA1/'
MYBOOK='/mnt/sde1-MyBook/'
#rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"
rsync -vaxEhPu --delete-before "${SOURCE}"       "${MIRROR}"
rsync -vaxEhPu --delete-before "${SOURCE}/data/" "${MYBOOK}/Data/"

