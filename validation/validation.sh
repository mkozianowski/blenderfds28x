#!/bin/bash

# usage: bash /path/to/validation.sh
/opt/blender/blender-2.80-linux-glibc217-x86_64/blender --background --python $(dirname $0)/validation.py
