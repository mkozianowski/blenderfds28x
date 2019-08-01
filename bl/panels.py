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
from bpy.types import Panel, UIList, Operator, bpy_struct

from ..lang import namelists
from ..lib import custom_uilist

bl_classes = list()
bf_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    if issubclass(cls, bpy_struct):
        bl_classes.append(cls)
    else:
        bf_classes.append(cls)
    return cls


# Property panels


class SCENE_PT_bf_namelist:
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "FDS Namelist"
    bl_context = "scene"

    bf_namelist_cls = "SN_HEAD"  # example
    layout = None  # example

    def draw_header(self, context):
        sc = context.scene
        bf_namelist = namelists[self.bf_namelist_cls]
        if bf_namelist.bpy_export:
            self.layout.prop(sc, bf_namelist.bpy_export, icon_only=True)
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"

    def draw(self, context):  # FIXME
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(
            row_major=True, columns=0, even_columns=True, even_rows=False, align=False
        )
        sc = context.scene
        bf_namelist = namelists[self.bf_namelist_cls]
        if bf_namelist.bpy_export:
            flow.active = getattr(sc, bf_namelist.bpy_export, True)
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_namelist_HEAD(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_HEAD"

    def draw(self, context):
        super().draw(context)
        row = self.layout.row()
        row.operator("scene.bf_show_fds_code", text="Show FDS Code", icon="HIDE_OFF")
        # row.operator("scene.bf_props_to_sel_obs", text="Copy To")  # FIXME


@subscribe
class SCENE_PT_bf_case_config(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_TIME(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_TIME"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_MISC(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_MISC"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_REAC(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_REAC"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_RADI(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_RADI"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_DUMP(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_DUMP"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_CATF(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_CATF"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class OBJECT_PT_bf_namelist(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS Namelist"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH" and not ob.bf_is_tmp

    def draw_header(self, context):
        ob = context.object
        bf_namelist = namelists[ob.bf_namelist_cls]
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ob, "hide_render", emboss=False, icon_only=True)

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        flow = layout.grid_flow(
            row_major=True, columns=0, even_columns=True, even_rows=False, align=False
        )
        ob = context.object
        layout.active = not (ob.hide_render)
        flow.prop(ob, "bf_namelist_cls")
        # Get the namelist class, instanciate it, and draw its panel
        namelists[ob.bf_namelist_cls](ob).draw(context, flow)
        row = flow.split(factor=0.5, align=True)
        if ob.bf_has_tmp:
            row.operator("object.bf_hide_fds_geometry", icon="HIDE_ON")
        else:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_OFF")
        row.operator("object.bf_show_fds_code", text="Show FDS Code", icon="HIDE_OFF")
        # row.operator("object.bf_props_to_sel_obs", text="Copy To")  # FIXME


@subscribe
class MATERIAL_PT_bf_namelist(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS SURF Namelist"

    @classmethod
    def poll(cls, context):
        ma = context.object.active_material
        return ma and ma.name not in {"INERT", "HVAC", "MIRROR", "OPEN", "PERIODIC"}

    def draw_header(self, context):  # FIXME
        ma = context.object.active_material
        bf_namelist = namelists[ma.bf_namelist_cls]
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ma, "bf_export", icon_only=True)

    def draw(self, context):  # FIXME
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(
            row_major=True, columns=0, even_columns=True, even_rows=False, align=False
        )
        ma = context.object.active_material
        layout.active = ma.bf_export
        flow.prop(ma, "bf_namelist_cls")
        # Get the namelist class, instanciate it, and draw its panel
        namelists[ma.bf_namelist_cls](ma).draw(context, flow)
        row = flow.row()
        row.operator("material.bf_show_fds_code", text="Show FDS Code")
        # row.operator("object.bf_props_to_sel_obs", text="Copy To")  # FIXME


# Register


def register():
    for cls in bl_classes:
        bpy.utils.register_class(cls)


def unregister():
    for cls in bl_classes:
        bpy.utils.unregister_class(cls)
