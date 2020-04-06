#!/bin/bash
# Usage: bash /path/to/verification.sh

# System variables
BLENDER_PATH="/opt/blender"
FDS_PATH="/opt/FDS/FDS6/bin"	

source $FDS_PATH/FDS6VARS.sh
$BLENDER_PATH/blender --background --python $(dirname $0)/verification.py 2> error.log
# rm error.log
