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
from bpy.types import (
    bpy_struct,
    PropertyGroup,
    UIList,
    Operator,
    Object,
    Scene,
    Material,
    Collection,
)
from bpy.props import (
    BoolProperty,
    FloatProperty,
    IntProperty,
    IntVectorProperty,
    StringProperty,
    PointerProperty,
    EnumProperty,
    CollectionProperty,
)

from ..lib.exceptions import BFException

# Collections

bl_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    bl_classes.append(cls)
    return cls


# GEOM, check geometry quality and intersections

from ..geometry.calc_trisurfaces import check_intersections, check_mesh_quality


@subscribe
class SCENE_OT_bf_check_intersections(Operator):
    bl_label = "Check Intersections"
    bl_idname = "object.bf_geom_check_intersections"
    bl_description = (
        "Check self-intersections or intersections with other selected objects"
    )

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        obs = context.selected_objects
        if obs:
            obs.remove(ob)
        try:
            check_intersections(context, ob, obs)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        w.cursor_modal_restore()
        self.report({"INFO"}, "No intersection")
        return {"FINISHED"}


@subscribe
class SCENE_OT_bf_check_quality(Operator):
    bl_label = "Check Quality"
    bl_idname = "object.bf_geom_check_quality"
    bl_description = "Check if closed orientable manifold, with no degenerate geometry"

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        try:
            check_mesh_quality(context, ob)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        w.cursor_modal_restore()

        self.report({"INFO"}, "Geometry quality ok")
        return {"FINISHED"}


# Show FDS code


class _COMMON_bf_show_fds_code:
    def draw(self, context):
        layout = self.layout
        bf_fds_code = self.bf_fds_code or "No FDS code is exported"
        for line in bf_fds_code.split("\n"):
            row = layout.row()
            row.label(text=line)

    def execute(self, context):
        self.report({"INFO"}, "FDS Code Shown")
        return {"FINISHED"}

    def _get_fds_code(self, context):
        self.bf_fds_code = ""

    def invoke(self, context, event):
        # Init
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        # Get the FDS code
        try:
            self._get_fds_code(context)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        # Call dialog
        w.cursor_modal_restore()
        wm = context.window_manager
        return wm.invoke_props_dialog(self, width=600)


@subscribe
class OBJECT_OT_bf_show_fds_code(_COMMON_bf_show_fds_code, Operator):
    bl_label = "Show FDS Code From Blender Object"
    bl_idname = "object.bf_show_fds_code"
    bl_description = "Show FDS code exported from Blender Object"

    @classmethod
    def poll(cls, context):
        return context.active_object

    def _get_fds_code(self, context):
        ob = context.active_object
        self.bf_fds_code = ob.to_fds(context)


@subscribe
class MATERIAL_OT_bf_show_fds_code(_COMMON_bf_show_fds_code, Operator):
    bl_label = "Show FDS Code From Blender Material"
    bl_idname = "material.bf_show_fds_code"
    bl_description = "Show FDS code exported from Blender Material"

    @classmethod
    def poll(cls, context):
        return context.active_object and context.active_object.active_material

    def _get_fds_code(self, context):
        ma = context.active_object.active_material
        self.bf_fds_code = ma.to_fds(context)


@subscribe
class SCENE_OT_bf_show_fds_code(_COMMON_bf_show_fds_code, Operator):
    bl_label = "Show FDS Code From Blender Scene"
    bl_idname = "scene.bf_show_fds_code"
    bl_description = "Show FDS code exported from Blender SCene"

    @classmethod
    def poll(cls, context):
        return context.scene

    def _get_fds_code(self, context):
        sc = context.scene
        self.bf_fds_code = sc.to_fds(context)


# Dialog box operator


@subscribe
class WM_OT_bf_dialog(Operator):
    bl_label = "BlenderFDS"
    bl_idname = "wm.bf_dialog"
    bl_description = "BlenderFDS Dialog"

    type: EnumProperty(
        name="Type",
        items=(("INFO", "Information", "Information"), ("ERROR", "Error", "Error")),
        description="Dialog type",
        default="INFO",
    )

    msg: StringProperty(
        name="Message", description="Dialog message", default="No message"
    )

    description: StringProperty(name="Description", description="Dialog description")

    def execute(self, context):
        return {"FINISHED"}

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def draw(self, context):
        layout = self.layout
        col = layout.column()
        col.label(text=self.msg, icon=self.type)
        if self.description:
            col.separator()
            descriptions = self.description.splitlines()
            for description in descriptions:
                row = col.row()
                row.label(description)


# Register


def register():
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)


def unregister():
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
