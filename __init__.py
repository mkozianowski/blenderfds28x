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

bl_info = {
    "name": "BlenderFDS 280",
    "author": "Emanuele Gissi",
    "description": "BlenderFDS, an open graphical editor for the NIST Fire Dynamics Simulator",
    "blender": (2, 80, 0),
    "version": (5, 0, 0),
    "location": "File > Export > FDS Case (.fds)",
    "warning": "",
    "category": "Import-Export",
    "wiki_url": "http://www.blenderfds.org/",
    "tracker_url": "https://github.com/firetools/blenderfds/issues",
    "support": "COMMUNITY",
}


# Register

from . import lang
from . import bl


def register():
    lang.register()
    bl.operators.register()
    bl.panels.register()
    bl.menus.register()
    bl.ui.register()


def unregister():
    #    bl.ui.unregister() # FIXME
    bl.menus.unregister()
    bl.panels.unregister()
    bl.operators.unregister()
    lang.unregister()

