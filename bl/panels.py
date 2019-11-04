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

from .. import lang
from . import custom_uilist
from .. import config
from .. import gis

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
    bl_label = "FDS Panel"
    bl_context = "scene"

    bf_namelist_cls = "SN_HEAD"  # example
    layout = None  # example

    @classmethod
    def poll(cls, context):
        return context.scene

    def draw_header(self, context):
        sc = context.scene
        # Manage Scene
        bf_namelist = lang.bf_namelists_by_cls[self.bf_namelist_cls]
        if bf_namelist.bpy_export:
            self.layout.prop(sc, bf_namelist.bpy_export, icon_only=True)
        if bf_namelist.description:
            self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        else:
            self.bl_label = bf_namelist.label

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist = lang.bf_namelists_by_cls[self.bf_namelist_cls]
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_case_config(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_config"

    def draw(self, context):
        row = self.layout.row(align=True)
        row.operator("scene.bf_show_fds_code", text="FDS Code", icon="HIDE_OFF")
        row.operator("scene.bf_props_to_scene", text="Copy To", icon="COPYDOWN")
        super().draw(context)


@subscribe
class SCENE_PT_bf_config_geoloc(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_config_geoloc"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_config_sizes(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_config_sizes"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_config_units(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_config_units"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_HEAD(Panel, SCENE_PT_bf_namelist):
    bf_namelist_cls = "SN_HEAD"


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
    bl_label = "FDS Panel"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"

    def draw_header(self, context):
        ob = context.object
        # Manage tmp Object
        if ob.bf_is_tmp:
            self.bl_label = "FDS Temporary Object"
            return
        # Manage Object
        bf_namelist = ob.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ob, "hide_render", emboss=False, icon_only=True)

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        # Operators
        row = flow.row(align=True)
        if ob.bf_is_tmp:
            row.operator("object.bf_hide_fds_geometry", icon="HIDE_ON")
            return
        if ob.bf_has_tmp:
            row.operator(
                "object.bf_hide_fds_geometry", icon="HIDE_ON", text="Hide Geometry"
            )
        else:
            row.operator(
                "object.bf_show_fds_geometry", icon="HIDE_OFF", text="Show Geometry"
            )
        row.operator("object.bf_show_fds_code", text="FDS Code", icon="HIDE_OFF")
        row.operator("object.bf_props_to_sel_obs", text="Copy To", icon="COPYDOWN")
        # Manage Object
        flow.prop(ob, "bf_namelist_cls")  # draw namelist choice
        ob.bf_namelist.draw(context, flow)  # draw namelist


@subscribe
class MATERIAL_PT_bf_namelist(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS Panel"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.active_material

    def draw_header(self, context):
        ma = context.object.active_material
        # Manage default Material
        if ma.name in config.default_mas:
            self.bl_label = f"FDS Predefined {ma.name}"
            return
        # Manage Material
        bf_namelist = ma.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ma, "bf_surf_export", icon_only=True)

    def draw(self, context):
        ma = context.object.active_material
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        # Manage default Material
        if ma.name in config.default_mas:
            flow.prop(ma, "diffuse_color")
            return
        # Operators
        row = flow.row(align=True)
        row.operator("material.bf_show_fds_code", text="FDS Code", icon="HIDE_OFF")
        row.operator("material.bf_surf_to_sel_obs", text="Assign To", icon="COPYDOWN")
        # Manage Material
        flow.prop(ma, "bf_namelist_cls")  # draw namelist choice
        ma.bf_namelist.draw(context, flow)  # draw namelist


# Toolbar panels


class BF_GEOM_Toolbar:
    bl_category = "FDS"
    bl_label = "GEOM Inspect"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        return ob and ob.type == "MESH" and ob.bf_namelist_cls == "ON_GEOM"

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(
            row_major=True, columns=0, even_columns=True, even_rows=False, align=False
        )
        ob = context.active_object
        flow.prop(ob, "bf_geom_protect")
        flow.operator("object.bf_geom_check_intersections")
        flow.operator("object.bf_geom_check_sanity")


@subscribe
class VIEW3D_PT_BF_GEOM_Object(Panel, BF_GEOM_Toolbar):
    bl_idname = "VIEW3D_PT_bf_geom_object"
    bl_context = "objectmode"


@subscribe
class VIEW3D_PT_BF_GEOM_Mesh(Panel, BF_GEOM_Toolbar):
    bl_idname = "VIEW3D_PT_bf_geom_mesh"
    bl_context = "mesh_edit"


class BF_Remesh_Toolbar:
    bl_category = "FDS"
    bl_label = "Remesh"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        return ob and ob.type == "MESH"

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        me = context.object.data
        flow = layout.grid_flow(
            row_major=True, columns=0, even_columns=True, even_rows=False, align=False
        )
        flow.label(text=f"Verts: {len(me.vertices)} | Faces: {len(me.polygons)}")
        flow.menu("VIEW3D_MT_edit_mesh_select_by_trait")
        flow.menu("VIEW3D_MT_edit_mesh_clean")
        flow.separator()
        flow.operator("object.manifold")
        flow.operator("object.quadriflow")
        flow.operator("object.simplify")


@subscribe
class VIEW3D_PT_BF_Remesh_Toolbar_Object(Panel, BF_Remesh_Toolbar):
    bl_idname = "VIEW3D_PT_bf_remesh_toolbar_object"
    bl_context = "objectmode"


@subscribe
class VIEW3D_PT_BF_Remesh_Toolbar_Mesh(Panel, BF_Remesh_Toolbar):
    bl_idname = "VIEW3D_PT_bf_remesh_toolbar_mesh"
    bl_context = "mesh_edit"


class BF_Geoloc_Toolbar:
    bl_category = "FDS"
    bl_label = "Geolocation"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    def draw(self, context):
        cursor = context.scene.cursor
        ob = context.active_object
        layout = self.layout
        col = layout.column(align=True)
        col.prop(cursor, "location", text="Cursor Location")
        row = col.row(align=True)
        row.operator("scene.bf_set_cursor_geoloc").show = False
        row.operator("scene.bf_set_cursor_geoloc", text="", icon="URL").show = True
        if not ob:
            return
        col = layout.column(align=True)
        col.prop(ob, "location", text="Item Location")
        row = col.row(align=True)
        row.operator("scene.bf_set_ob_geoloc").show = False
        row.operator("scene.bf_set_ob_geoloc", text="", icon="URL").show = True


@subscribe
class VIEW3D_PT_BF_Geoloc_Toolbar_Object(Panel, BF_Geoloc_Toolbar):
    bl_idname = "VIEW3D_PT_bf_geoloc_toolbar_object"
    bl_context = "objectmode"


@subscribe
class VIEW3D_PT_BF_Geoloc_Toolbar_Mesh(Panel, BF_Geoloc_Toolbar):
    bl_idname = "VIEW3D_PT_bf_geoloc_toolbar_mesh"
    bl_context = "mesh_edit"


# Register


def register():
    for cls in bl_classes:
        bpy.utils.register_class(cls)


def unregister():
    for cls in bl_classes:
        bpy.utils.unregister_class(cls)
