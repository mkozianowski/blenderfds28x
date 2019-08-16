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
    "name": "BlenderFDS",
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


# Setup logging

import logging

logsFormat = "%(levelname)s:%(name)s:%(lineno)d:%(message)s"
logging.basicConfig(level=logging.getLevelName("INFO"), format=logsFormat)

# Use like this FIXME:
# import logging
# log = logging.getLogger(__name__)
# log.warning('Origin proj has been deleted because the property could not be updated', exc_info=True)
# log.error('Cannot update crs', exc_info=True)
# self.report({'ERROR'}, 'Cannot update crs. Check logs form more info')
# return {'CANCELLED'}
# log.info("Read shapefile...")

# Register

import bpy

from . import lang
from .bl import operators, panels, menus, ui, handlers, preferences


def register():
    preferences.register()

    # Set log level
    pref = bpy.context.preferences.addons[__package__].preferences
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.getLevelName(pref.bf_loglevel))

    lang.register()
    operators.register()
    panels.register()
    menus.register()
    handlers.register()
    if bpy.context.preferences.addons["blenderfds28x"].preferences.bf_pref_simplify_ui:
        ui.register()


def unregister():
    # ui.unregister() # FIXME for now, restart needed
    menus.unregister()
    panels.unregister()
    operators.unregister()
    lang.unregister()
    handlers.unregister()
    preferences.unregister()

