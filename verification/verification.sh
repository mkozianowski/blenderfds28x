#!/bin/bash

#system variables
BLENDER_PATH="/opt/blender"
FDS_PATH="/opt/FDS/FDS6/bin"	

# usage: bash /path/to/verification.sh
source $FDS_PATH/FDS6VARS.sh
$BLENDER_PATH/blender --background --python $(dirname $0)/verification.py 2> log.error
rm log.error
