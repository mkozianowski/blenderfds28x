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
from .. import geometry
from ..lang import OP_XB, OP_XYZ, OP_PB

# Collections

bl_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    bl_classes.append(cls)
    return cls


# GEOM, check geometry quality and intersections


@subscribe
class OBJECT_OT_bf_check_intersections(Operator):
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
            geometry.calc_trisurfaces.check_intersections(context, ob, obs)
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "No intersection")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


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
            geometry.calc_trisurfaces.check_mesh_quality(context, ob)
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "Geometry quality ok")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


# Show FDS code


class _show_fds_code:
    def draw(self, context):
        lines = self.lines.split("\n") or ("No FDS code is exported",)
        if len(lines) > 20:
            lines = lines[:20]
            lines.append("...")
        layout = self.layout
        for line in lines:
            layout.label(text=line)

    def execute(self, context):
        self.report({"INFO"}, "FDS Code Shown")
        return {"FINISHED"}

    def _get_lines(self, context):
        return str()

    def invoke(self, context, event):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        try:
            self.lines = self._get_lines(context)  # get FDS code
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            wm = context.window_manager
            return wm.invoke_props_dialog(self, width=600)
        finally:
            w.cursor_modal_restore()


@subscribe
class OBJECT_OT_bf_show_fds_code(_show_fds_code, Operator):
    bl_label = "Show FDS Code From Current Object"
    bl_idname = "object.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Object"

    @classmethod
    def poll(cls, context):
        return context.object

    def _get_lines(self, context):
        return context.object.to_fds(context)


@subscribe
class MATERIAL_OT_bf_show_fds_code(_show_fds_code, Operator):
    bl_label = "Show FDS Code From Current Material"
    bl_idname = "material.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Material"

    @classmethod
    def poll(cls, context):
        return context.active_object and context.active_object.active_material

    def _get_lines(self, context):
        ma = context.active_object.active_material
        self.lines = ma.to_fds(context)


@subscribe
class SCENE_OT_bf_show_fds_code(_show_fds_code, Operator):
    bl_label = "Show FDS Code From Current Scene"
    bl_idname = "scene.bf_show_fds_code"
    bl_description = "Show FDS code exported from Scene"

    @classmethod
    def poll(cls, context):
        return context.scene

    def _get_lines(self, context):
        return context.scene.to_fds(context)


# Show exported geometry


@subscribe
class OBJECT_OT_bf_show_fds_geometry(Operator):
    bl_label = "Show FDS Geometry"
    bl_idname = "object.bf_show_fds_geometry"
    bl_description = "Show geometry as exported to FDS"

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.object
        # GEOM
        if ob.bf_namelist_cls == "ON_GEOM" and ob.bf_export:
            try:
                fds_surfids, fds_verts, fds_faces, msg = geometry.to_fds.ob_to_geom(
                    context, ob, check=ob.bf_geom_check_quality
                )
            except BFException as err:
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                ob_tmp = geometry.from_fds.geom_to_ob(
                    fds_surfids,
                    fds_verts,
                    fds_faces,
                    context,
                    name=f"Tmp Object {ob.name} GEOM",
                )
                geometry.utils.set_tmp_object(context, ob_tmp, ob)
                self.report({"INFO"}, msg)
                return {"FINISHED"}
            finally:
                w.cursor_modal_restore()
        # XB, XYZ, PB*
        msgs = list()
        if ob.bf_xb_export and OP_XB in ob.bf_namelist.param_cls:
            try:
                xbs, msg = geometry.to_fds.ob_to_xbs(context, ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.from_fds.xbs_to_ob(
                    xbs, context, bf_xb=ob.bf_xb, name=f"Tmp Object {ob.name} XBs"
                )
                geometry.utils.set_tmp_object(context, ob_tmp, ob)
        if ob.bf_xyz_export and OP_XYZ in ob.bf_namelist.param_cls:
            try:
                xyzs, msg = geometry.to_fds.ob_to_xyzs(context, ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.from_fds.xyzs_to_ob(
                    xyzs, context, bf_xyz=ob.bf_xyz, name=f"Tmp Object {ob.name} XYZs"
                )
                geometry.utils.set_tmp_object(context, ob_tmp, ob)
        if ob.bf_pb_export and OP_PB in ob.bf_namelist.param_cls:
            try:
                pbs, msg = geometry.to_fds.ob_to_pbs(context, ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.from_fds.pbs_to_ob(
                    pbs, context, bf_pb=ob.bf_pb, name=f"Tmp Object {ob.name} PBs"
                )
                geometry.utils.set_tmp_object(context, ob_tmp, ob)
        w.cursor_modal_restore()
        self.report(
            {"INFO"}, "; ".join(msg for msg in msgs if msg) or "Geometry exported."
        )
        return {"FINISHED"}


@subscribe
class OBJECT_OT_bf_hide_fds_geometry(Operator):
    bl_label = "Hide FDS Geometry"
    bl_idname = "object.bf_hide_fds_geometry"
    bl_description = "Hide geometry as exported to FDS"

    def execute(self, context):
        geometry.utils.rm_tmp_objects(context)
        self.report({"INFO"}, "FDS geometry hidden")
        return {"FINISHED"}


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
