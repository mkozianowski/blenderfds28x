"""!
BlenderFDS, <a href="https://docs.blender.org/api/current/bpy.types.Panel.html">panel</a> class extensions
"""

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

from . import custom_uilist
from .. import lang, config, geometry, gis, fds

bl_classes = list()
bf_classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collection.
    @param cls: the class to subscribe.
    @return the class subscribed.
    """
    if issubclass(cls, bpy_struct):
        bl_classes.append(cls)
    else:
        bf_classes.append(cls)
    return cls


# Property panels


class SCENE_PT_bf_namelist:
    """!
    FDS Panel
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "FDS Panel"
    bl_context = "scene"

    bf_namelist_cls = "SN_HEAD"  # example
    layout = None  # example

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return current scene
        """
        return context.scene

    def draw_header(self, context):
        """!
        Draw UI elements into the panel’s header UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
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
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist = lang.bf_namelists_by_cls[self.bf_namelist_cls]
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_case_config(Panel, SCENE_PT_bf_namelist):
    """!
    FDS Case Config
    """

    bf_namelist_cls = "SN_config"
    bl_label = "FDS Case Config"

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        row = layout.row(align=True)  # general operators
        row.operator("scene.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("scene.bf_props_to_scene", icon="COPYDOWN")
        bf_namelist = lang.bf_namelists_by_cls[self.bf_namelist_cls]
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_config_geoloc(Panel, SCENE_PT_bf_namelist):
    """!
    Origin Geolocation
    """

    bf_namelist_cls = "SN_config_geoloc"
    bl_label = "Origin Geolocation"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_config_sizes(Panel, SCENE_PT_bf_namelist):
    """!
    Default Sizes and Thresholds
    """

    bf_namelist_cls = "SN_config_sizes"
    bl_label = "Default Sizes and Thresholds"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_config_units(Panel, SCENE_PT_bf_namelist):
    """!
    Units configuration
    """

    bf_namelist_cls = "SN_config_units"
    bl_label = "Units"
    bl_parent_id = "SCENE_PT_bf_case_config"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_HEAD(Panel, SCENE_PT_bf_namelist):
    """!
    FDS HEAD
    """

    bf_namelist_cls = "SN_HEAD"
    bl_label = "FDS HEAD"


@subscribe
class SCENE_PT_bf_namelist_TIME(Panel, SCENE_PT_bf_namelist):
    """!
    FDS TIME
    """

    bf_namelist_cls = "SN_TIME"
    bl_label = "FDS TIME"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_MISC(Panel, SCENE_PT_bf_namelist):
    """!
    FDS MISC
    """

    bf_namelist_cls = "SN_MISC"
    bl_label = "FDS MISC"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_REAC(Panel, SCENE_PT_bf_namelist):
    """!
    FDS REAC
    """

    bf_namelist_cls = "SN_REAC"
    bl_label = "FDS REAC"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_RADI(Panel, SCENE_PT_bf_namelist):
    """!
    FDS RADI
    """

    bf_namelist_cls = "SN_RADI"
    bl_label = "FDS RADI"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_DUMP(Panel, SCENE_PT_bf_namelist):
    """!
    FDS DUMP
    """

    bf_namelist_cls = "SN_DUMP"
    bl_label = "FDS DUMP"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_CATF(Panel, SCENE_PT_bf_namelist):
    """!
    FDS CATF
    """

    bf_namelist_cls = "SN_CATF"
    bl_label = "FDS CATF"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class OBJECT_PT_bf_namelist(Panel):
    """!
    FDS Geometric Namelist
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS Geometric Namelist"

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return current object
        """
        ob = context.object
        return ob and ob.type == "MESH"

    def draw_header(self, context):
        """!
        Draw UI elements into the panel’s header UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        ob = context.object
        if ob.bf_is_tmp:
            self.bl_label = f"FDS Temp Geometry"
            return
        bf_namelist = ob.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ob, "hide_render", emboss=False, icon_only=True)

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        row = layout.row(align=True)  # general operators
        if ob.bf_is_tmp:
            row.operator("object.bf_hide_fds_geometry", icon="HIDE_ON")
            return
        if ob.bf_has_tmp:
            row.operator("object.bf_hide_fds_geometry", icon="HIDE_ON")
        else:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_OFF")
        row.operator("object.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("object.bf_props_to_sel_obs", icon="COPYDOWN")
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        flow.prop(ob, "bf_namelist_cls")  # draw namelist choice
        ob.bf_namelist.draw(context, flow)  # draw namelist


@subscribe
class MATERIAL_PT_bf_namelist(Panel):
    """!
    FDS SURF
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS SURF"

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return current object
        """
        ob = context.object
        return ob and ob.active_material

    def draw_header(self, context):
        """!
        Draw UI elements into the panel’s header UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        ma = context.object.active_material
        # Manage predefined Material
        if ma.name in config.default_mas:
            self.bl_label = f"FDS Predefined {ma.name}"
            return
        # Manage Material
        bf_namelist = ma.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ma, "bf_surf_export", icon_only=True)

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        ma = context.object.active_material
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        row = layout.row(align=True)  # general operators
        if ma.name in config.default_mas:
            row.prop(ma, "diffuse_color")
            return
        row.operator("material.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("material.bf_surf_to_sel_obs", icon="COPYDOWN")
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        flow.prop(ma, "bf_namelist_cls")  # draw namelist choice
        ma.bf_namelist.draw(context, flow)  # draw namelist


# Toolbar panels


@subscribe
class VIEW3D_PT_BF_Object_Tools(Panel):
    """!
    Object Tools
    """

    bl_idname = "VIEW3D_PT_bf_object_tools"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "Object Tools"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        return context.object

    def draw(self, context):
        layout = self.layout
        ob = context.object
        col = layout.column(align=True)
        # Tmp object
        if ob.bf_is_tmp:
            col.operator("object.bf_hide_fds_geometry", icon="HIDE_ON")  # FIXME
            return
        # Geolocation
        col = layout.column(align=True)
        col.prop(ob, "location")
        row = col.row(align=True)
        row.operator("scene.bf_set_ob_geoloc").show = False
        row.operator("scene.bf_set_ob_geoloc", text="", icon="URL").show = True
        # GEOM operators
        if ob.bf_namelist_cls == "ON_GEOM":
            col = layout.column(align=True)
            col.label(text="FDS GEOM:")
            col.operator("object.bf_geom_check_intersections")
            col.operator("object.bf_geom_check_sanity")
            box = col.box().column(align=True)
            box.prop(ob, "bf_geom_protect")
            me = ob.data
            material_slots = ob.material_slots
            box.label(
                text=f"SURF_ID: {len(material_slots)} | VERTS: {len(me.vertices)} | FACES: {len(me.polygons)}"
            )
        # MESH operators
        if ob.bf_namelist_cls == "ON_MESH":
            col = layout.column(align=True)
            col.label(text="FDS MESH:")
            col.operator("object.bf_set_mesh_cell_size")
            box = col.box().column(align=True)
            scale_length = context.scene.unit_settings.scale_length
            xbs = geometry.utils.get_bbox_xbs(
                context=context, ob=ob, scale_length=scale_length
            )
            has_good_ijk, cs, cell_count, cell_aspect_ratio = fds.mesh_tools.calc_cell_infos(
                ijk=ob.bf_mesh_ijk, xbs=xbs
            )
            box.label(text=f"Size: {cs[0]:.3f}m, {cs[1]:.3f}m, {cs[2]:.3f}m")
            box.label(
                text=f"Qty: {cell_count} | Aspect: {cell_aspect_ratio:.1f} | Poisson: {has_good_ijk and 'Yes' or 'No'}"
            )
            col.operator("object.bf_align_selected_meshes")


@subscribe
class VIEW3D_PT_BF_Fix_Toolbar_Object(Panel):
    """!
    Object Remesh
    """

    # See: properties_data_mesh.py, class DATA_PT_remesh

    bl_idname = "VIEW3D_PT_bf_fix_toolbar_object"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "Object Remesh"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return current object
        """
        ob = context.active_object
        return ob and ob.type == "MESH"

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        layout = self.layout
        ob = context.active_object
        me = ob.data
        col = layout.column()
        col.label(
            text=f"{ob.name} | Verts: {len(me.vertices)} | Faces: {len(me.polygons)}"
        )
        row = col.row()
        row.prop(me, "remesh_mode", text="Mode", expand=True)
        if me.remesh_mode == "VOXEL":
            col.prop(me, "remesh_voxel_size")
            col.prop(me, "remesh_voxel_adaptivity")
            col.prop(me, "use_remesh_fix_poles")
            col.prop(me, "use_remesh_smooth_normals")
            col.prop(me, "use_remesh_preserve_volume")
            # col.prop(me, "use_remesh_preserve_paint_mask")
            col.operator("object.voxel_remesh", text="Voxel Remesh")
        else:
            col.operator("object.quadriflow_remesh", text="QuadriFlow Remesh")


@subscribe
class VIEW3D_PT_BF_Fix_Toolbar_Mesh(Panel):
    """!
    Mesh Clean Up
    """

    bl_idname = "VIEW3D_PT_bf_fix_toolbar_mesh"
    bl_context = "mesh_edit"
    bl_category = "FDS"
    bl_label = "Mesh Clean Up"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return current object
        """
        ob = context.active_object
        return ob and ob.type == "MESH"

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        ob = context.active_object
        me = ob.data
        col = layout.column()
        col.label(
            text=f"{ob.name} | Verts: {len(me.vertices)} | Faces: {len(me.polygons)}"
        )
        row = col.row(align=True)
        row.template_header_3D_mode()
        row.menu("VIEW3D_MT_edit_mesh_select_by_trait")
        col.menu("VIEW3D_MT_edit_mesh_clean")


@subscribe
class VIEW3D_PT_BF_view3d_cursor(Panel):
    """!
    3D Cursor
    """

    # See: space_view3d.py, class VIEW3D_PT_view3d_cursor

    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "FDS"
    bl_label = "3D Cursor"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        layout = self.layout
        cursor = context.scene.cursor
        col = layout.column(align=True)
        col.prop(cursor, "location", text="Location")
        row = col.row(align=True)
        row.operator("scene.bf_set_ob_geoloc").show = False
        row.operator("scene.bf_set_ob_geoloc", text="", icon="URL").show = True


# Register


def register():
    """!
    Load the Python classes and functions to blender.
    """
    for cls in bl_classes:
        bpy.utils.register_class(cls)


def unregister():
    """!
    Unload the Python classes and functions from blender.
    """
    for cls in bl_classes:
        bpy.utils.unregister_class(cls)
