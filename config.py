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

# TODO
# from . import bl_info
# module = bl_info['name']
# PKG = __package__
# mod = addon_utils.addons_fake_modules.get(PKG)
# mod.bl_info['show_expanded'] = True

# Supported file version

supported_file_version = 5, 0, 0

# FDS default SURFs: name, diffuse_color
default_mas = {
    "Dummy Color1": ((1.0, 1.0, 1.0, 0.05),),  # white
    "Dummy Color2": ((1.0, 1.0, 0.0, 0.05),),  # yellow
    "Dummy Color3": ((1.0, 0.0, 1.0, 0.05),),  # purple
    "INERT": ((0.8, 0.8, 0.2, 1.0),),
    "HVAC": ((0.2, 0.2, 0.8, 0.5),),
    "MIRROR": ((1.0, 0.0, 1.0, 0.2),),
    "OPEN": ((0.2, 0.8, 0.8, 0.05),),
    "PERIODIC": ((1.0, 0.0, 1.0, 0.2),),
}
