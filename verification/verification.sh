#!/bin/bash
# Usage: bash /path/to/verification.sh

# System variables
BLENDER_PATHFILE="/opt/blender/blender"
FDS_PATH="/opt/FDS/FDS6/bin"	

# FDS setup
echo "FDS setup..."
ulimit -s unlimited
ulimit -v unlimited
source $FDS_PATH//FDS6VARS.sh
source $FDS_PATH//SMV6VARS.sh
export OMP_NUM_THREADS=1

# Running Blender
echo "Run verification.py in Blender..."
$BLENDER_PATHFILE --background --python $(dirname $0)/verification.py 2> error.log
#rm error.log
