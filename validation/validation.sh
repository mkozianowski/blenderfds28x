#!/bin/bash

# usage: bash /path/to/validation.sh
blender --background --python $(dirname $0)/validation.py
