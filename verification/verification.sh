#!/bin/bash

#system variables
BLENDER_PATH="/home/egissi/github/blender"
FDS_PATH="/home/egissi/FDS/FDS6/bin"

# usage: bash /path/to/verification.sh
source $FDS_PATH/FDS6VARS.sh
$BLENDER_PATH/blender --background --python $(dirname $0)/verification.py
