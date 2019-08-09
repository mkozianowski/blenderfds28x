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
from ..lib import config
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
            geometry.calc_trisurfaces.check_intersections(
                context, ob, obs, protect=ob.bf_geom_protect
            )
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
            geometry.calc_trisurfaces.check_geom_quality(
                context, ob, protect=ob.bf_geom_protect
            )
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "Geometry quality ok")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


# GEOM remesh

import subprocess


@subscribe
class OBJECT_OT_quadriflow_remesh(Operator):
    bl_idname = "object.quadriflow_remesh"
    bl_label = "Quadriflow Remesh"

    bl_options = {"REGISTER", "UNDO"}

    resolution: bpy.props.IntProperty(
        name="Resolution",
        description="Octree resolution",
        min=5,
        max=10000,
        default=300,
    )
    sharp: bpy.props.BoolProperty(
        name="Sharp edges",
        description="Detect and perserve the sharp edges",
        default=True,
    )
    mcf: bpy.props.BoolProperty(
        name="Min-cost flow",
        description="Adaptive network simplex minimum-cost flow solver",
        default=False,
    )

    @classmethod
    def poll(cls, context):
        # FIXME no binary, OBJECT mode, SCULPT
        return context.object

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "resolution")
        layout.prop(self, "sharp")
        layout.prop(self, "mcf")

    def execute(self, context):
        bf_quadriflow_filepath = (
            "/home/egissi/github/blenderfds28x/bin/hjwdzh/quadriflow"
        )  # bpy.context.preferences.addons[
        ob = context.object
        input_path = os.path.join(bpy.app.tempdir, f"{ob.name}.obj")
        output_path = os.path.join(bpy.app.tempdir, f"{ob.name}_quadflow.obj")
        bpy.ops.export_scene.obj(
            filepath=input_path,
            check_existing=True,
            use_selection=True,
            use_mesh_modifiers=False,
            use_normals=True,
            use_uvs=True,
            use_materials=False,
            use_triangles=True,
            path_mode="AUTO",
        )
        quadriflow_command = [
            bf_quadriflow_filepath,
            "-i",
            input_path,
            "-o",
            output_path,
            "-f",
            str(self.resolution),
        ]
        if self.mcf:
            quadriflow_command.append("-mcf")
        if self.sharp:
            quadriflow_command.append("-sharp")
        print("Command:", quadriflow_command)
        try:
            subprocess.run(quadriflow_command, check=True)
        except subprocess.CalledProcessError:
            self.report({"ERROR"}, "Subprocess error")  # FIXME specify
            return {"CANCELLED"}
        #        bpy.ops.object.delete() FIXME
        bpy.ops.import_scene.obj(filepath=output_path)
        return {"FINISHED"}


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
        return context.active_object.active_material.to_fds(context)


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
        scale_length = context.scene.unit_settings.scale_length
        # GEOM
        if ob.bf_namelist_cls == "ON_GEOM" and ob.bf_export:
            check = ob.bf_geom_check_quality
            try:
                fds_surfids, fds_verts, fds_faces, msg = geometry.to_fds.ob_to_geom(
                    context, ob, scale_length, check
                )
            except BFException as err:
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                ob_tmp = geometry.utils.get_tmp_object(
                    context, ob, f"{ob.name}_GEOM_tmp"
                )
                geometry.from_fds.geom_to_ob(
                    fds_surfids, fds_verts, fds_faces, context, ob_tmp, scale_length
                )
                self.report({"INFO"}, msg)
                return {"FINISHED"}
            finally:
                w.cursor_modal_restore()
        # XB, XYZ, PB*
        msgs = list()
        if ob.bf_xb_export and OP_XB in ob.bf_namelist.param_cls:  # XB
            try:
                xbs, msg = geometry.to_fds.ob_to_xbs(context, ob, scale_length)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(context, ob, f"{ob.name}_XB_tmp")
                geometry.from_fds.xbs_to_ob(
                    xbs, context, ob_tmp, scale_length, ob.bf_xb
                )
        if ob.bf_xyz_export and OP_XYZ in ob.bf_namelist.param_cls:  # XYZ
            try:
                xyzs, msg = geometry.to_fds.ob_to_xyzs(context, ob, scale_length)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(
                    context, ob, f"{ob.name}_XYZ_tmp"
                )
                geometry.from_fds.xyzs_to_ob(xyzs, context, ob_tmp, scale_length)
        if ob.bf_pb_export and OP_PB in ob.bf_namelist.param_cls:  # PB
            try:
                pbs, msg = geometry.to_fds.ob_to_pbs(context, ob, scale_length)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(
                    context, ob, f"{ob.name}_PB*_tmp"
                )
                geometry.from_fds.pbs_to_ob(pbs, context, ob_tmp, scale_length)
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


# Copy FDS parameters between Blender elements

from .. import lang


def _bf_props_copy(context, source_element, dest_elements):
    """Copy all parameters from source_element to dest_elements"""
    for _, param in lang.params.items():
        # Get value
        if not isinstance(source_element, param.bpy_type):
            continue
        bpy_idname = param.bpy_idname
        if not bpy_idname or param.bf_other.get("copy_protect"):
            continue
        bpy_value = getattr(source_element, bpy_idname)
        # Set value
        print(f"BFDS: Copy: {source_element.name} ->")
        if isinstance(bpy_value, bpy.types.bpy_prop_collection):
            for dest_element in dest_elements:
                dest_coll = getattr(dest_element, bpy_idname)
                dest_coll.clear()
                for source_item in bpy_value:
                    dest_item = dest_coll.add()
                    for k, v in source_item.items():
                        dest_item[k] = v
                        print(f"BFDS:   -> {dest_element.name}: {bpy_idname}: {k}={v}")
        else:
            for dest_element in dest_elements:
                setattr(dest_element, bpy_idname, bpy_value)
                print(f"BFDS:   -> {dest_element.name}: {bpy_idname}={bpy_value}")


@subscribe
class SCENE_OT_bf_copy_props_to_scene(Operator):
    bl_label = "Copy To Scene"
    bl_idname = "scene.bf_props_to_scene"
    bl_description = "Copy all current scene FDS parameters to another Scene"

    bf_destination_element: StringProperty(name="Destination Scene")

    def draw(self, context):
        layout = self.layout
        row = layout.row()
        row.prop_search(
            self, "bf_destination_element", bpy.data, "scenes", text="Scene"
        )

    def execute(self, context):
        # Get source and destination scenes
        source_element = context.scene
        destination_elements = (
            bpy.data.scenes.get(self.bf_destination_element, None),
        )  # a tuple!
        if not destination_elements[0]:
            self.report({"WARNING"}, "No destination Scene")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Scene")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, destination_elements)
        self.report({"INFO"}, "Copied to destination Scene")
        return {"FINISHED"}

    def invoke(self, context, event):
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


@subscribe
class OBJECT_OT_bf_copy_FDS_properties_to_sel_obs(Operator):
    bl_label = "Copy To Selected Objects"
    bl_idname = "object.bf_props_to_sel_obs"
    bl_description = "Copy current object FDS parameters to selected Objects"

    def invoke(self, context, event):  # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        if context.mode != "OBJECT":
            bpy.ops.object.mode_set(mode="OBJECT", toggle=False)
        # Get source and destination objects
        source_element = context.active_object
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, destination_elements)
        self.report({"INFO"}, "Copied to selected Objects")
        return {"FINISHED"}


@subscribe
class MATERIAL_OT_bf_assign_BC_to_sel_obs(Operator):
    bl_label = "Assign To Selected Objects"
    bl_idname = "material.bf_surf_to_sel_obs"
    bl_description = "Assign current boundary condition to selected Objects"

    def invoke(self, context, event):  # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT", toggle=False)
        # Get source and destination materials
        source_element = context.active_object
        active_material = source_element.active_material
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        if not active_material:
            self.report({"WARNING"}, "No boundary condition to assign")
            return {"CANCELLED"}
        # Loop on objects
        for ob in destination_elements:
            ob.active_material = active_material
            print(
                "BlenderFDS: Assign Material '{}' -> {}".format(
                    active_material.name, ob.name
                )
            )
        # Set myself as exported
        active_material.bf_export = True
        # Return
        self.report({"INFO"}, "Assigned to selected Objects")
        return {"FINISHED"}


# Register


def register():
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)


def unregister():
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
