#!/bin/bash

SOURCE='DLPBMM1:/home/localcosadmin/'
MIRROR='/home/localcosadmin.archive/'
rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"

SOURCE='DLPBMM1:/home/localcosadmin.bk/'
MIRROR='/home/localcosadmin.bk.archive/'
rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"

SOURCE='DLPBMM1:/home/mi164210/'
MIRROR='/home/mi164210.archive/'
rsync -vaxEhPu --inplace "${SOURCE}" "${MIRROR}"
