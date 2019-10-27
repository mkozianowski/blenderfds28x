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

import os

import bpy, logging
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty, FloatProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper

from .. import utils
from ..types import BFException, FDSCase

log = logging.getLogger(__name__)


# Collections

bl_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    bl_classes.append(cls)
    return cls


# Import menus


@subscribe
class ImportFDS(Operator, ImportHelper):
    """Import FDS case file to a Scene"""

    bl_idname = "import_scene.fds"
    bl_label = "Import FDS"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})
    new_scene: BoolProperty(name="Into New Scene", default=True)

    @classmethod
    def poll(cls, context):
        return context.scene is not None

    def execute(self, context):
        # Init
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        # Read and parse
        fds_case = FDSCase()
        try:
            fds_case.from_fds(utils.read_from_file(self.filepath))
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Read or parse error: {str(err)}")
            return {"CANCELLED"}
        # Current or new Scene
        if self.new_scene:
            sc = bpy.data.scenes.new("Imported")
        else:
            sc = context.scene
        # Import
        try:
            sc.from_fds(context, fds_case=fds_case)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Import error: {str(err)}")
            return {"CANCELLED"}
        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, "FDS case imported")
        return {"FINISHED"}


def menu_func_import_FDS(self, context):
    self.layout.operator(
        "import_scene.fds", text="NIST FDS (.fds) into New Scene"
    ).new_scene = True


def menu_func_import_snippet_FDS(self, context):
    self.layout.operator(
        "import_scene.fds", text="NIST FDS (.fds) into Current Scene"
    ).new_scene = False


# Export menu


@subscribe
class ExportFDS(Operator, ExportHelper):
    """Export current Scene to FDS case file"""

    bl_idname = "export_scene.fds"
    bl_label = "Export FDS"
    bl_description = "Export current Blender Scene as an FDS case file"

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene is not None

    def execute(self, context):
        # Init
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        sc = context.scene
        # Prepare FDS filepath
        filepath = self.filepath
        if not filepath.lower().endswith(".fds"):
            filepath += ".fds"
        filepath = bpy.path.abspath(filepath)
        log.debug(f"Exporting Blender Scene <{sc.name}> to <{filepath}>...")
        # Test FDS filepath is writable, before calculations
        if not utils.write_to_file(filepath):
            w.cursor_modal_restore()
            self.report({"ERROR"}, "Filepath not writable, cannot export")
            return {"CANCELLED"}
        # Prepare FDS file
        try:
            fds_file = sc.to_fds(context=context, full=True)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Error while assembling FDS file:\n<{str(err)}>")
            return {"CANCELLED"}
        # Write FDS file
        try:
            utils.write_to_file(filepath, fds_file)
        except IOError:
            w.cursor_modal_restore()
            self.report({"ERROR"}, "FDS case filepath not writable, cannot export")
            return {"CANCELLED"}
        # GE1 description file requested?
        if sc.bf_dump_render_file:
            # Prepare GE1 filepath
            filepath = filepath[:-4] + ".ge1"
            # Prepare GE1 file
            try:
                ge1_file = sc.to_ge1(context=context)
            except BFException as err:
                w.cursor_modal_restore()
                self.report(
                    {"ERROR"}, f"Error while assembling GE1 file:\n<{str(err)}>"
                )
                return {"CANCELLED"}
            # Write GE1 file
            if not utils.write_to_file(filepath, ge1_file):
                w.cursor_modal_restore()
                self.report({"ERROR"}, "GE1 filepath not writable, cannot export")
                return {"CANCELLED"}
        # End
        w.cursor_modal_restore()
        log.debug(f"Exporting done!")
        self.report({"INFO"}, "FDS case exported")
        return {"FINISHED"}

    def draw(self, context):
        pass


def menu_func_export_FDS(self, context):
    # Init
    sc = context.scene
    basename = "{0}.fds".format(bpy.path.clean_name(sc.name))
    directory = ""
    # Prepare default filepath
    if bpy.data.filepath:
        directory = os.path.dirname(bpy.data.filepath)
    # If the context scene contains path and basename, use them
    if sc.bf_config_directory:
        directory = sc.bf_config_directory
    # Call the exporter operator
    if directory:
        self.layout.operator(
            "export_scene.fds", text="NIST FDS (.fds)"
        ).filepath = f"{directory}/{basename}"
    else:
        self.layout.operator(
            "export_scene.fds", text="NIST FDS (.fds)"
        ).filepath = basename


# Register


def register():
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_snippet_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_FDS)


def unregister():
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_snippet_FDS)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_FDS)
