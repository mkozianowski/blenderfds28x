# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

import bpy


# Shortcut

module = "blenderfds28x"


def get_prefs():
    return bpy.context.preferences.addons[module].preferences


# Default separator
separator = "\n      "


# Default comment format
def comment(msg):
    return "! {}\n".format(msg)


# FDS default SURFs: name, diffuse_color
default_mas = {
    "INERT": ((0.8, 0.8, 0.2, 1.0),),
    "HVAC": ((0.2, 0.2, 0.8, 1.0),),
    "MIRROR": ((0.2, 0.2, 0.8, 0.2),),
    "OPEN": ((0.2, 0.8, 0.8, 0.2),),
    "PERIODIC": ((0.8, 0.8, 0.8, 0.2),),
}
