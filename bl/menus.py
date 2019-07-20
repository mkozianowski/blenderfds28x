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

import bpy
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty, FloatProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper

from ..lib.utils import is_writable, write_to_file
from ..lib.types import BFException

# Collections

bl_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    bl_classes.append(cls)
    return cls


# Import menu


@subscribe
class ImportFDS(Operator, ImportHelper):
    """Import FDS case file to a new Scene"""

    bl_idname = "import_scene.fds"
    bl_label = "Import FDS"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})

    @classmethod
    def poll(cls, context):
        return context.scene is not None

    def execute(self, context):
        return {"CANCELLED"}


def menu_func_import_FDS(self, context):
    self.layout.operator("import_scene.fds", text="NIST FDS (.fds)")


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
        print(f"BFDS: Exporting Blender Scene <{sc.name}> to FDS file...")
        filepath = self.filepath
        if not filepath.lower().endswith(".fds"):
            filepath += ".fds"
        filepath = bpy.path.abspath(filepath)
        # Check FDS filepath writable
        if not is_writable(filepath):
            w.cursor_modal_restore()
            self.report({"ERROR"}, "FDS file not writable, cannot export")
            return {"CANCELLED"}
        # Prepare FDS file
        try:
            fds_file = sc.to_fds(context=context, full=True)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        # Add namelist index # TODO develop
        # Write FDS file
        if not write_to_file(filepath, fds_file):
            w.cursor_modal_restore()
            self.report({"ERROR"}, "FDS file not writable, cannot export")
            return {"CANCELLED"}
        print(f"BFDS: FDS file written.")
        # GE1 description file requested?
        if sc.bf_dump_render_file:
            print(
                f"BFDS: Warning: Exporting Blender Scene <{sc.name}> to GE1 file not implemented!"
            )  # FIXME
            # # Prepare GE1 filepath
            # print(f"BFDS: Exporting Blender Scene <{sc.name}> to GE1 file...")
            # filepath = filepath[:-4] + ".ge1"
            # if not is_writable(filepath):
            #     w.cursor_modal_restore()
            #     self.report({"ERROR"}, "GE1 file not writable, cannot export")
            #     return {"CANCELLED"}
            # # Prepare GE1 file
            # try:
            #     ge1_file = sc.to_ge1(context=context)
            # except BFException as err:
            #     w.cursor_modal_restore()
            #     self.report({"ERROR"}, str(err))
            #     return {"CANCELLED"}
            # # Write GE1 file
            # if not write_to_file(filepath, ge1_file):
            #     w.cursor_modal_restore()
            #     self.report({"ERROR"}, "GE1 file not writable, cannot export")
            #     return {"CANCELLED"}
            # print(f"BFDS: GE1 file written.")
        # End
        w.cursor_modal_restore()
        self.report({"INFO"}, "FDS case exported")
        return {"FINISHED"}

    def draw(self, context):
        pass


def menu_func_export_FDS(self, context):
    # Prepare default filepath
    filepath = "{0}.fds".format(os.path.splitext(bpy.data.filepath)[0])
    directory = os.path.dirname(filepath)
    basename = os.path.basename(filepath)
    # If the context scene contains path and basename, use them
    sc = context.scene
    if sc.bf_head_directory:
        directory = sc.bf_head_directory
    if sc.name:
        basename = "{0}.fds".format(bpy.path.clean_name(sc.name))
    # Call the exporter operator
    filepath = f"{directory}/{basename}"
    self.layout.operator("export_scene.fds", text="Scene to NIST FDS (.fds)")


# Register


def register():
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)
    #    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)  # FIXME implement
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_FDS)


def unregister():
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
    #    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)  # FIXME implement
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_FDS)
