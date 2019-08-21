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

import re, os.path, time, sys, logging

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
from . import geometry
from .types import BFException, Parameter, Namelist, PString, PFYI, POthers
from .config import separator, comment, default_mas
from . import gis

log = logging.getLogger(__name__)


# Collections

namelists = dict()
params = dict()
bl_classes = list()
bf_classes = list()


def subscribe(cls):
    """Subscribe class to related collection."""
    if issubclass(cls, Namelist):
        namelists[cls.__name__] = cls
    elif issubclass(cls, Parameter):
        params[cls.__name__] = cls
    elif issubclass(cls, bpy_struct):
        bl_classes.append(cls)
    else:
        bf_classes.append(cls)
    return cls


# PropertyGroup and UIList


@subscribe
class WM_PG_bf_others(PropertyGroup):
    bf_export: BoolProperty(name="Export", default=False)
    name: StringProperty(name="Name")


@subscribe
class WM_UL_bf_others_items(UIList):
    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


@subscribe
class WM_PG_bf_filepaths(PropertyGroup):
    bf_export: BoolProperty(name="Export", default=False)
    name: StringProperty(name="Name", subtype="FILE_PATH")


@subscribe
class WM_UL_bf_filepaths_items(UIList):
    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


# HEAD


@subscribe
class SP_HEAD_CHID(Parameter):
    label = "CHID"
    description = "Case identificator"
    fds_label = "CHID"
    bf_other = {"copy_protect": True}
    bpy_type = Scene
    bpy_idname = "name"

    def check(self, context):
        value = self.element.name
        if value and bpy.path.clean_name(value) != value:
            raise BFException(self, "Illegal characters in case filename")


@subscribe
class SP_HEAD_TITLE(PFYI):
    label = "TITLE"
    description = "Case description"
    fds_label = "TITLE"
    bpy_type = Scene
    bpy_idname = "bf_head_title"
    bpy_other = {"maxlen": 64}


@subscribe
class SN_HEAD(Namelist):
    label = "HEAD"
    description = "Case header"
    enum_id = 3001
    fds_label = "HEAD"
    bpy_type = Scene
    bpy_export = "bf_head_export"
    bpy_export_default = True
    param_cls = SP_HEAD_CHID, SP_HEAD_TITLE


# Case Config


@subscribe
class SP_config_directory(Parameter):
    label = "Case Directory"
    description = "Destination directory for exported case"
    bpy_type = Scene
    bpy_idname = "bf_head_directory"
    bpy_prop = StringProperty
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    def check(self, context):
        value = self.element.bf_head_directory
        if value and not os.path.exists(bpy.path.abspath(value)):
            raise BFException(self, "Case directory path not existing")


@subscribe
class SP_config_min_edge_length(Parameter):
    label = "Min Edge Length"
    description = "Min allowed edge length"
    bpy_type = Scene
    bpy_idname = "bf_config_min_edge_length"
    bpy_prop = FloatProperty
    bpy_default = 1e-05
    bpy_other = {"unit": "LENGTH"}


@subscribe
class SP_config_min_face_area(Parameter):
    label = "Min Face Area"
    description = "Min allowed face area"
    bpy_type = Scene
    bpy_idname = "bf_config_min_face_area"
    bpy_prop = FloatProperty
    bpy_default = 1e-05
    bpy_other = {"unit": "AREA"}


@subscribe
class SP_config_default_voxel_size(Parameter):
    label = "Voxel/Pixel Size"
    description = "Default voxel/pixel resolution"
    bpy_type = Scene
    bpy_idname = "bf_default_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {"unit": "LENGTH", "step": 1.0, "precision": 3}

@subscribe
class SP_utm_zn(Parameter):
    label = "UTM Zone Number (WGS84)"
    description = "UTM zone number (WGS84 datum) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_utm_zn"  # not compatible with BlenderGIS
    bpy_prop = IntProperty
    bpy_default = 32  # Monte Fasce, Genova, Italy
    bpy_other = {"min": 1, "max": 60}

    def draw(self, context, layout):
        sc = self.element
        row = layout.row(align=True)
        row.label(text="UTM Zone")
        row.prop(sc, "bf_utm_zn", text="")
        row.prop(sc, "bf_utm_ze", text="")

@subscribe
class SP_utm_ze(Parameter):
    label = "UTM Zone Emisphere (WGS84)"
    description = "UTM zone emisphere (WGS84 datum) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_utm_ze"  # not compatible with BlenderGIS
    bpy_prop = EnumProperty
    bpy_default = "N"  # Monte Fasce, Genova, Italy
    bpy_other = {
        "items": (("N", "N", "Northern Emisphere"), ("S", "S", "Southern Emisphere"))
    }

    def draw(self, context, layout):
        pass

@subscribe
class SP_utm_e(Parameter):
    label = "UTM Easting (WGS84)"
    description = "UTM easting (WGS84 datum) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_utm_e"  # not compatible with BlenderGIS
    bpy_prop = IntProperty
    bpy_default = 502742  # Monte Fasce, Genova, Italy
    bpy_other = {"min": 100000, "max": 900000}


@subscribe
class SP_utm_n(Parameter):
    label = "UTM Northing (WGS84)"
    description = "UTM northing (WGS84 datum) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_utm_n"  # not compatible with BlenderGIS
    bpy_prop = IntProperty
    bpy_default = 4917346  # Monte Fasce, Genova, Italy
    bpy_other = {"min": 0, "max": 10000000}


# FIXME This is not compatible with FDS convention for WIND namelist
@subscribe
class SP_config_north_heading(Parameter):
    label = "World North Heading"
    description = "Angle between +y axis and north heading"
    bpy_type = Scene
    bpy_idname = "bf_north_heading"  # not compatible with BlenderGIS
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"unit": "ROTATION", "precision": 4}


# FIXME This is not compatible with FDS convention for pressure?
@subscribe
class SP_config_height(Parameter):
    label = "World Origin Height"
    description = "Height of world origin"
    bpy_type = Scene
    bpy_idname = "bf_height"  # not compatible with BlenderGIS
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"unit": "LENGTH", "precision": 4}


@subscribe
class SN_config(Namelist):
    label = "Config"
    description = "Case configuration"
    enum_id = 3008
    bpy_type = Scene

    def draw(self, context, layout):
        sc = self.element
        col = layout.column()
        col.prop(sc, "bf_head_directory")
        col.separator()

        col = col.column()
        sub = col.column(align=True)
        row = sub.row(align=True)
        row.prop(sc, "bf_utm_zn", text="World Origin UTM Zone")
        row.prop(sc, "bf_utm_ze", text="")
        row.operator("scene.bf_set_origin_pos", text="", icon="IMPORT")
        utm = gis.UTM(sc.bf_utm_zn, True, sc.bf_utm_e, sc.bf_utm_n)
        row.operator("wm.url_open", text="", icon="URL").url = utm.to_url()
        sub.prop(sc, "bf_utm_e", text="UTM Easting")
        sub.prop(sc, "bf_utm_n", text="UTM Northing")
        col.prop(sc, "bf_height")
        col.prop(sc, "bf_north_heading")

        col.separator()
        col.prop(sc, "bf_config_min_edge_length")
        col.prop(sc, "bf_config_min_face_area")
        col.prop(sc, "bf_default_voxel_size")
        col.separator()
        unit = sc.unit_settings
        col.prop(unit, "system")
        col = col.column()
        col.enabled = unit.system != "NONE"
        col.prop(unit, "scale_length")
        col.prop(unit, "use_separate")
        col.prop(unit, "length_unit", text="Length")
        # col.prop(unit, "mass_unit", text="Mass")  # Unused
        col.prop(unit, "time_unit", text="Time")

    def to_fds(self, context):
        return


# TIME


@subscribe
class SP_TIME_setup_only(Parameter):
    label = "Smokeview Geometry Setup"
    description = "Set Smokeview to setup only geometry"
    bpy_type = Scene
    bpy_idname = "bf_time_setup_only"
    bpy_prop = BoolProperty
    bpy_default = False


@subscribe
class SP_TIME_T_BEGIN(Parameter):
    label = "T_BEGIN [s]"
    description = "Simulation starting time"
    fds_label = "T_BEGIN"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_time_t_begin"
    bpy_prop = FloatProperty
    bpy_other = {"unit": "TIME", "step": 100.0, "precision": 1}

    @property
    def exported(self):
        return super().exported and not self.element.bf_time_setup_only


@subscribe
class SP_TIME_T_END(Parameter):
    label = "T_END [s]"
    description = "Simulation ending time"
    fds_label = "T_END"
    bpy_type = Scene
    bpy_idname = "bf_time_t_end"
    bpy_prop = FloatProperty
    bpy_default = 1.0
    bpy_other = {"unit": "TIME", "step": 100.0, "precision": 1}

    @property
    def exported(self):
        return super().exported and not self.element.bf_time_setup_only


@subscribe
class SP_TIME_other(POthers):
    bpy_type = Scene
    bpy_idname = "bf_time_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class SN_TIME(Namelist):
    label = "TIME"
    description = "Simulation time settings"
    enum_id = 3002
    fds_label = "TIME"
    bpy_type = Scene
    bpy_export = "bf_time_export"
    bpy_export_default = True
    param_cls = SP_TIME_T_BEGIN, SP_TIME_T_END, SP_TIME_other

    def draw(self, context, layout):
        sc = self.element
        layout.prop(sc, "bf_time_setup_only")
        sub = layout.column()
        sub.active = not sc.bf_time_setup_only
        SP_TIME_T_BEGIN(sc).draw(context, sub)
        SP_TIME_T_END(sc).draw(context, sub)
        SP_TIME_other(sc).draw(context, layout)


# MISC


@subscribe
class SP_MISC_FYI(PFYI):
    bpy_type = Scene
    bpy_idname = "bf_misc_fyi"


@subscribe
class SP_MISC_OVERWRITE(Parameter):
    label = "OVERWRITE"
    description = "Do not check for the existence of CHID.out and overwrite files"
    fds_label = "OVERWRITE"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_overwrite"


@subscribe
class SP_MISC_THICKEN_OBSTRUCTIONS(Parameter):
    label = "THICKEN_OBSTRUCTIONS"
    description = "Do not allow thin sheet obstructions"
    fds_label = "THICKEN_OBSTRUCTIONS"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_thicken_obstructions"


@subscribe
class SP_MISC_other(POthers):
    bpy_type = Scene
    bpy_idname = "bf_misc_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class SN_MISC(Namelist):
    label = "MISC"
    description = "Miscellaneous parameters"
    enum_id = 3003
    fds_label = "MISC"
    bpy_type = Scene
    bpy_export = "bf_misc_export"
    bpy_export_default = False
    param_cls = (
        SP_MISC_FYI,
        SP_MISC_OVERWRITE,
        SP_MISC_THICKEN_OBSTRUCTIONS,
        SP_MISC_other,
    )


# REAC


@subscribe
class SP_REAC_FUEL(PString):
    label = "FUEL"
    description = "Identificator of fuel species"
    fds_label = "FUEL"
    bpy_type = Scene
    bpy_idname = "bf_reac_fuel"


@subscribe
class SP_REAC_FYI(PFYI):
    bpy_type = Scene
    bpy_idname = "bf_reac_fyi"


@subscribe
class SP_REAC_FORMULA(PString):
    label = "FORMULA"
    description = "Chemical formula of fuel species, it can only contain C, H, O, or N"
    fds_label = "FORMULA"
    bpy_type = Scene
    bpy_idname = "bf_reac_formula"
    bpy_export = "bf_reac_formula_export"
    bpy_export_default = True


@subscribe
class SP_REAC_CO_YIELD(Parameter):
    label = "CO_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into carbon monoxide"
    fds_label = "CO_YIELD"
    fds_default = 0.0
    bpy_type = Scene
    bpy_prop = FloatProperty
    bpy_idname = "bf_reac_co_yield"
    bpy_other = {"step": 1.0, "precision": 3, "min": 0.0, "max": 1.0}
    bpy_export = "bf_reac_co_yield_export"
    bpy_export_default = False


@subscribe
class SP_REAC_SOOT_YIELD(SP_REAC_CO_YIELD):
    label = "SOOT_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into smoke particulate"
    fds_label = "SOOT_YIELD"
    bpy_type = Scene
    bpy_idname = "bf_reac_soot_yield"
    bpy_export = "bf_reac_soot_yield_export"
    bpy_export_default = False


@subscribe
class SP_REAC_HEAT_OF_COMBUSTION(Parameter):
    label = "HEAT_OF_COMBUSTION [kJ/kg]"
    description = "Fuel heat of combustion"
    fds_label = "HEAT_OF_COMBUSTION"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_reac_heat_of_combustion"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 1, "min": 0.0}
    bpy_export = "bf_reac_heat_of_combustion_export"
    bpy_export_default = False


@subscribe
class SP_REAC_IDEAL(Parameter):
    label = "IDEAL"
    description = "Set ideal heat of combustion"
    fds_label = "IDEAL"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_reac_ideal"


@subscribe
class SP_REAC_other(POthers):
    bpy_type = Scene
    bpy_idname = "bf_reac_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class SN_REAC(Namelist):
    label = "REAC"
    description = "Reaction"
    enum_id = 3004
    fds_label = "REAC"
    bpy_type = Scene
    bpy_export = "bf_reac_export"
    param_cls = (
        SP_REAC_FUEL,
        SP_REAC_FYI,
        SP_REAC_FORMULA,
        SP_REAC_CO_YIELD,
        SP_REAC_SOOT_YIELD,
        SP_REAC_HEAT_OF_COMBUSTION,
        SP_REAC_IDEAL,
        SP_REAC_other,
    )


# RADI


@subscribe
class SP_RADI_FYI(PFYI):
    bpy_type = Scene
    bpy_idname = "bf_radi_fyi"


@subscribe
class SP_RADI_RADIATION(Parameter):
    label = "RADIATION"
    description = "Turn on/off the radiation solver"
    fds_label = "RADIATION"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_radi_radiation"


@subscribe
class SP_RADI_RADIATIVE_FRACTION(Parameter):
    label = "RADIATIVE_FRACTION"
    description = (
        "Fraction of the total combustion energy that is released "
        "in the form of thermal radiation"
    )
    fds_label = "RADIATIVE_FRACTION"
    fds_default = 0.35
    bpy_type = Scene
    bpy_idname = "bf_radi_radiative_fraction"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 2, "min": 0.0, "max": 1.0}


@subscribe
class SP_RADI_NUMBER_RADIATION_ANGLES(Parameter):
    label = "NUMBER_RADIATION_ANGLES"
    description = "Number of angles for spatial resolution of radiation solver"
    fds_label = "NUMBER_RADIATION_ANGLES"
    fds_default = 100
    bpy_type = Scene
    bpy_idname = "bf_radi_number_radiation_angles"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_TIME_STEP_INCREMENT(Parameter):
    label = "TIME_STEP_INCREMENT"
    description = "Frequency of calls to the radiation solver in time steps"
    fds_label = "TIME_STEP_INCREMENT"
    fds_default = 3
    bpy_type = Scene
    bpy_idname = "bf_radi_time_step_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_ANGLE_INCREMENT(Parameter):
    label = "ANGLE_INCREMENT"
    description = "Increment over which the angles are updated"
    fds_label = "ANGLE_INCREMENT"
    fds_default = 5
    bpy_type = Scene
    bpy_idname = "bf_radi_angle_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_RADIATION_ITERATIONS(Parameter):
    label = "RADIATION_ITERATIONS"
    description = "Number of times the radiative intensity is updated in a time step"
    fds_label = "RADIATION_ITERATIONS"
    fds_default = 1
    bpy_type = Scene
    bpy_idname = "bf_radi_radiation_iterations"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_other(POthers):
    bpy_type = Scene
    bpy_idname = "bf_radi_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class SN_RADI(Namelist):
    label = "RADI"
    description = "Radiation parameters"
    enum_id = 3006
    fds_label = "RADI"
    bpy_type = Scene
    bpy_export = "bf_radi_export"
    bpy_export_default = False
    param_cls = (
        SP_RADI_FYI,
        SP_RADI_RADIATION,
        SP_RADI_RADIATIVE_FRACTION,
        SP_RADI_NUMBER_RADIATION_ANGLES,
        SP_RADI_TIME_STEP_INCREMENT,
        SP_RADI_ANGLE_INCREMENT,
        SP_RADI_RADIATION_ITERATIONS,
        SP_RADI_other,
    )


# DUMP


@subscribe
class SP_DUMP_FYI(PFYI):
    bpy_type = Scene
    bpy_idname = "bf_dump_fyi"


@subscribe
class SP_DUMP_render_file(Parameter):
    label = "Export Geometric Description File"
    description = "Export geometric description file GE1"
    fds_label = "RENDER_FILE"
    bpy_type = Scene
    bpy_idname = "bf_dump_render_file"
    bpy_prop = BoolProperty
    bpy_default = True

    @property
    def value(self):
        if self.element.bf_dump_render_file:
            return f"{self.element.name}.ge1"


@subscribe
class SP_DUMP_STATUS_FILES(Parameter):
    label = "STATUS_FILES"
    description = "Export status file (*.notready), deleted when the simulation is completed successfully"
    fds_label = "STATUS_FILES"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_dump_status_files"


@subscribe
class SP_DUMP_NFRAMES(Parameter):
    label = "NFRAMES"
    description = "Number of output dumps per calculation"
    fds_label = "NFRAMES"
    fds_default = 1000
    bpy_type = Scene
    bpy_idname = "bf_dump_nframes"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_DUMP_set_frequency(Parameter):
    label = "Dump Output every 1 s"
    description = "Dump output every 1 s"
    bpy_type = Scene
    bpy_idname = "bf_dump_set_frequency"
    bpy_prop = BoolProperty
    bpy_default = False

    def to_fds(self, context):
        return


@subscribe
class SP_DUMP_DT_RESTART(Parameter):
    label = "DT_RESTART"
    description = "Time interval between restart files are saved"
    fds_label = "DT_RESTART"
    fds_default = 600
    bpy_type = Scene
    bpy_idname = "bf_dump_dt_restart"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_DUMP_other(POthers):
    bpy_type = Scene
    bpy_idname = "bf_dump_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class SN_DUMP(Namelist):
    label = "DUMP"
    description = "Output parameters"
    enum_id = 3005
    fds_label = "DUMP"
    bpy_type = Scene
    bpy_export = "bf_dump_export"
    bpy_export_default = False
    param_cls = (
        SP_DUMP_FYI,
        SP_DUMP_render_file,
        SP_DUMP_STATUS_FILES,
        SP_DUMP_NFRAMES,
        SP_DUMP_set_frequency,
        SP_DUMP_DT_RESTART,
        SP_DUMP_other,
    )


# CATF

# FIXME to_fds
@subscribe
class SP_CATF_check_files(Parameter):
    label = "Check File Existance While Exporting"
    description = (
        "Check file existence and export filepaths relative to the case directory"
    )
    bpy_type = Scene
    bpy_idname = "bf_catf_check_files"
    bpy_prop = BoolProperty
    bpy_default = True


@subscribe
class SP_CATF_files(POthers):  # FIXME
    label = "Concatenated Files"
    description = "Concatenated files (eg. PROP='/drive/test.catf')"
    bpy_type = Scene
    bpy_idname = "bf_catf_files"
    bpy_pg = WM_PG_bf_filepaths
    bpy_ul = WM_UL_bf_filepaths_items

    def draw(self, context, layout):
        sc = self.element
        SP_CATF_check_files(sc).draw(context, layout)
        super().draw(context, layout)


@subscribe
class SN_CATF(Namelist):  # FIXME
    label = "CATF"
    description = "Concatenated file paths"
    fds_label = "CATF"
    bpy_type = Scene
    bpy_export = "bf_catf_export"
    bpy_export_default = False
    param_cls = (SP_CATF_files,)


# Material


def update_MP_namelist_cls(self, context):
    # When Material namelist cls is updated:
    # Set default appearance
    # self.set_default_appearance(context)
    pass


@subscribe
class MP_namelist_cls(Parameter):
    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Material
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (("MN_SURF", "SURF", "Generic boundary condition", 2000),),
        "update": update_MP_namelist_cls,
    }
    bpy_default = "MN_SURF"

    def to_fds(self, context):
        if self.element.name in {"INERT", "HVAC", "MIRROR", "OPEN", "PERIODIC"}:
            return
        super().to_fds(context)


@subscribe
class MP_ID(PString):
    label = "ID"
    description = "Material identification name"
    fds_label = "ID"
    bf_other = {"copy_protect": True}
    bpy_type = Material
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"


@subscribe
class MP_FYI(PFYI):
    bpy_type = Material
    bpy_idname = "bf_fyi"


@subscribe
class MP_RGB(Parameter):
    label = "RGB, TRANSPARENCY"
    description = "Color values (red, green, blue) and transparency"
    fds_label = "RGB"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def to_fds(self, context):
        c = self.element.diffuse_color
        rgb = int(c[0] * 255), int(c[1] * 255), int(c[2] * 255)
        return f"RGB={rgb[0]},{rgb[1]},{rgb[2]} TRANSPARENCY={c[3]:.2f}"


@subscribe
class MP_THICKNESS(Parameter):
    label = "THICKNESS [m]"
    description = "Surface thickness for heat transfer calculation"
    fds_label = "THICKNESS"
    bpy_type = Material
    bpy_idname = "bf_thickness"
    bpy_prop = FloatProperty
    bpy_default = 0.01
    bpy_other = {"step": 1.0, "precision": 6, "min": 0.000001}
    bpy_export = "bf_thickness_export"
    bpy_export_default = False


@subscribe
class MP_HRRPUA(Parameter):
    label = "HRRPUA [kW/m²]"
    description = "Heat release rate per unit area"
    fds_label = "HRRPUA"
    bpy_type = Material
    bpy_idname = "bf_hrrpua"
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"precision": 3, "min": 0.0}


@subscribe
class MP_TAU_Q(Parameter):
    label = "TAU_Q [s]"
    description = "Ramp time for heat release rate"
    fds_label = "TAU_Q"
    fds_default = 1.0
    bpy_type = Material
    bpy_idname = "bf_tau_q"
    bpy_prop = FloatProperty
    bpy_other = {"step": 10.0, "precision": 1, "unit": "TIME"}


@subscribe
class MP_MATL_ID(PString):
    label = "MATL_ID"
    description = "Reference to a MATL (Material) line for self properties"
    fds_label = "MATL_ID"
    bpy_type = Material
    bpy_idname = "bf_matl_id"
    bpy_export = "bf_matl_id_export"
    bpy_export_default = False


@subscribe
class MP_IGNITION_TEMPERATURE(Parameter):
    label = "IGNITION_TEMPERATURE [°C]"
    description = "Ignition temperature"
    fds_label = "IGNITION_TEMPERATURE"
    fds_default = 5000.0
    bpy_type = Material
    bpy_idname = "bf_ignition_temperature"
    bpy_prop = FloatProperty
    bpy_other = {"step": 100.0, "precision": 1, "min": -273.0}
    bpy_export = "bf_ignition_temperature_export"
    bpy_export_default = False


@subscribe
class MP_BACKING(Parameter):
    label = "BACKING"
    description = "Exposition of back side surface"
    fds_label = "BACKING"
    fds_default = "EXPOSED"
    bpy_type = Material
    bpy_idname = "bf_backing"
    bpy_prop = EnumProperty
    bpy_prop_export = "bf_backing_export"
    bpy_export_default = False
    bpy_other = {
        "items": (
            (
                "VOID",
                "VOID",
                "The wall is assumed to back up to the ambient temperature",
            ),
            (
                "INSULATED",
                "INSULATED",
                "The back side of the material is perfectly insulated",
            ),
            (
                "EXPOSED",
                "EXPOSED",
                "The heat transfer into the space behind the wall is calculated (only if wall is one cell thick)",
            ),
        )
    }


@subscribe
class MP_other(POthers):
    bpy_type = Material
    bpy_idname = "bf_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class MN_SURF(Namelist):
    label = "SURF"
    description = "Generic boundary condition"
    enum_id = 2000
    bpy_type = Material
    fds_label = "SURF"
    bpy_export = "bf_export"
    bpy_export_default = True
    param_cls = (MP_ID, MP_FYI, MP_RGB, MP_MATL_ID, MP_THICKNESS, MP_BACKING, MP_other)

    @property
    def exported(self) -> "bool":
        return self.element.bf_export and self.element.name not in default_mas


@subscribe
class MN_SURF_burner(MN_SURF):
    label = "SURF Burner"
    description = "Spec'd rate burner"
    enum_id = 2001
    param_cls = MP_ID, MP_FYI, MP_RGB, MP_HRRPUA, MP_TAU_Q, MP_other


@subscribe
class MN_SURF_solid(MN_SURF):
    label = "SURF Solid"
    description = "Spec'd rate burning solid"
    enum_id = 2002
    param_cls = (
        MP_ID,
        MP_FYI,
        MP_RGB,
        MP_HRRPUA,
        MP_TAU_Q,
        MP_MATL_ID,
        MP_IGNITION_TEMPERATURE,
        MP_THICKNESS,
        MP_BACKING,
        MP_other,
    )


# Object


def update_OP_namelist_cls(ob, context):
    # When Object namelist cls is updated:
    ob.bf_xb = update_bf_xb_items(ob, context)[0][0]
    ob.bf_xyz = update_bf_xyz_items(ob, context)[0][0]
    ob.bf_pb = update_bf_pb_items(ob, context)[0][0]
    ob.bf_id_suffix = update_bf_id_suffix_items(ob, context)[0][0]
    # TODO set default appearance


@subscribe
class OP_namelist_cls(Parameter):
    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Object
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_default = "ON_OBST"
    bpy_other = {
        "items": (("ON_OBST", "OBST", "Obstruction", 1000),),
        "update": update_OP_namelist_cls,
    }


# OBST


@subscribe
class OP_ID(PString):
    label = "ID"
    description = "Object identification name"
    fds_label = "ID"
    bf_other = {"copy_protect": True}
    bpy_type = Object
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"


@subscribe
class OP_FYI(PFYI):
    bpy_type = Object
    bpy_idname = "bf_fyi"


def update_bf_xb(ob, context):
    pass  # FIXME del tmp
    if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES") and ob.bf_xb_export:
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


@subscribe
class OP_XB_custom_voxel(Parameter):
    label = "Use Custom Voxel/Pixel"
    description = "Use custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_custom_voxel"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}


@subscribe
class OP_XB_voxel_size(Parameter):
    label = "Custom Voxel/Pixel Size"
    description = "Custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {
        "step": 1.0,
        "precision": 3,
        "min": 0.001,
        "max": 20.0,
        "unit": "LENGTH",
        "update": update_bf_xb,
    }
    bpy_export = "bf_xb_custom_voxel"


@subscribe
class OP_XB_center_voxels(Parameter):
    label = "Center Voxels/Pixels"
    description = "Center voxels/pixels to Object bounding box"
    bpy_type = Object
    bpy_idname = "bf_xb_center_voxels"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}


def update_bf_xb_items(ob, context):
    return tuple(
        (
            ("BBOX", "BBox", "Export object bounding box"),
            ("VOXELS", "Voxels", "Export voxels from voxelized solid Object"),
            ("FACES", "Faces", "Export faces, one for each face"),
            ("PIXELS", "Pixels", "Export pixels from pixelized flat Object"),
            ("EDGES", "Edges", "Export segments, one for each edge"),
        )[i]
        for i in namelists[ob.bf_namelist_cls].bf_xb_idxs or (0, 1, 2, 3, 4)
    )


@subscribe
class OP_XB_export(Parameter):
    label = "Export XB"
    description = "Set if XB shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xb_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_xb}


@subscribe
class OP_XB(Parameter):
    label = "XB"
    description = "Export as volumes/faces"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = "bf_xb"
    bpy_prop = EnumProperty
    bpy_other = {"update": update_bf_xb, "items": update_bf_xb_items}
    bpy_export = "bf_xb_export"

    def draw(self, context, layout):
        super().draw(context, layout)
        ob = self.element
        if ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"):
            OP_XB_center_voxels(ob).draw(context, layout)
            OP_XB_voxel_size(ob).draw(context, layout)

    _format_xb = "XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}"
    _format_xbs = {
        "IDI": "ID='{1}_{2}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDX": "ID='{1}_X{0[0]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDY": "ID='{1}_Y{0[2]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDZ": "ID='{1}_Z{0[4]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDXY": "ID='{1}_X{0[0]:+.3f}_Y{0[2]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDXZ": "ID='{1}_X{0[0]:+.3f}_Z{0[4]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDYZ": "ID='{1}_Y{0[2]:+.3f}_Z{0[4]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
        "IDXYZ": "ID='{1}_X{0[0]:+.3f}_Y{0[2]:+.3f}_Z{0[4]:+.3f}'{3}XB={0[0]:.6f},{0[1]:.6f},{0[2]:.6f},{0[3]:.6f},{0[4]:.6f},{0[5]:.6f}",
    }

    def to_fds(self, context):
        ob = self.element
        if not ob.bf_xb_export:
            return
        scale_length = context.scene.unit_settings.scale_length
        xbs, msg = geometry.to_fds.ob_to_xbs(context, ob, scale_length)
        if not xbs:
            return None, msg
        elif len(xbs) == 1:
            return self._format_xb.format(xbs[0]), msg
        else:
            name = ob.name
            format_xbs = self._format_xbs[
                self.element.bf_id_suffix
            ]  # choose formatting string
            return (
                (format_xbs.format(xb, name, i, separator) for i, xb in enumerate(xbs)),
                msg,
            )


def update_bf_xyz(ob, context):
    if ob.bf_xyz == "VERTICES" and ob.bf_xyz_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


def update_bf_xyz_items(ob, context):
    return tuple(
        (
            ("CENTER", "Center", "Point, center point of this object"),
            ("VERTICES", "Vertices", "Points, one for each vertex of this object"),
        )[i]
        for i in namelists[ob.bf_namelist_cls].bf_xyz_idxs or (0, 1)
    )


@subscribe
class OP_XYZ_export(Parameter):
    label = "Export XYZ"
    description = "Set if XYZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xyz_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_xyz}


@subscribe
class OP_XYZ(Parameter):
    label = "XYZ"
    description = "Export as points"
    fds_label = "XYZ"
    bpy_type = Object
    bpy_idname = "bf_xyz"
    bpy_prop = EnumProperty
    bpy_other = {"update": update_bf_xyz, "items": update_bf_xyz_items}
    bpy_export = "bf_xyz_export"

    _format_xyz = "XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}"
    _format_xyzs = {
        "IDI": "ID='{1}_{2}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDX": "ID='{1}_X{0[0]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDY": "ID='{1}_Y{0[1]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDZ": "ID='{1}_Z{0[2]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDXY": "ID='{1}_X{0[0]:+.3f}_Y{0[1]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDXZ": "ID='{1}_X{0[0]:+.3f}_Z{0[2]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDYZ": "ID='{1}_Y{0[1]:+.3f}_Z{0[2]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
        "IDXYZ": "ID='{1}_X{0[0]:+.3f}_Y{0[1]:+.3f}_Z{0[2]:+.3f}'{3}XYZ={0[0]:.6f},{0[1]:.6f},{0[2]:.6f}",
    }

    def to_fds(self, context):
        ob = self.element
        if not ob.bf_xyz_export:
            return
        scale_length = context.scene.unit_settings.scale_length
        xyzs, msg = geometry.to_fds.ob_to_xyzs(context, ob, scale_length)
        if not xyzs:
            return None, msg
        elif len(xyzs) == 1:
            return self._format_xyz.format(xyzs[0]), msg
        else:
            name = ob.name
            format_xyzs = self._format_xyzs[
                self.element.bf_id_suffix
            ]  # choose formatting string
            return (
                (
                    format_xyzs.format(xyz, name, i, separator)
                    for i, xyz in enumerate(xyzs)
                ),
                msg,
            )


def update_bf_pb(ob, context):
    if ob.bf_pb == "PLANES" and ob.bf_pb_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        return


def update_bf_pb_items(ob, context):
    return (("PLANES", "Planes", "Planes, one for each face of this object"),)


@subscribe
class OP_PB_export(Parameter):
    label = "Export PBX, PBY, PBZ"
    description = "Set if PBX, PBY, PBZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_pb_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_pb}


@subscribe
class OP_PB(Parameter):
    label = "PBX, PBY, PBZ"
    description = "Export as planes"
    bpy_type = Object
    bpy_idname = "bf_pb"
    bpy_prop = EnumProperty
    bpy_other = {"update": update_bf_pb, "items": update_bf_pb_items}
    bpy_export = "bf_pb_export"

    _format_pb = ("PBX={0:.6f}", "PBY={0:.6f}", "PBZ={0:.6f}")
    _format_pbs = {
        "IDI": (
            "ID='{1}_{2}'{3}PBX={0:.6f}",
            "ID='{1}_{2}'{3}PBY={0:.6f}",
            "ID='{1}_{2}'{3}PBZ={0:.6f}",
        ),
        "IDXYZ": (
            "ID='{1}_X{0:+.3f}'{3}PBX={0:.6f}",
            "ID='{1}_Y{0:+.3f}'{3}PBY={0:.6f}",
            "ID='{1}_Z{0:+.3f}'{3}PBZ={0:.6f}",
        ),
    }

    def to_fds(self, context):
        ob = self.element
        if not ob.bf_pb_export:
            return
        scale_length = context.scene.unit_settings.scale_length
        pbs, msg = geometry.to_fds.ob_to_pbs(context, ob, scale_length)
        # pbs is: (0, 3.5), (0, 4.), (2, .5) ...
        # with 0, 1, 2 perpendicular axis
        if not pbs:
            return None, msg
        elif len(pbs) == 1:
            pb = pbs[0]
            return self._format_pb[pb[0]].format(pb[1]), msg
        else:
            name = ob.name
            if self.element.bf_id_suffix == "IDI":  # choose formatting string
                format_pbs = self._format_pbs["IDI"]
            else:
                format_pbs = self._format_pbs["IDXYZ"]
            return (
                (
                    format_pbs[pb[0]].format(pb[1], name, i, separator)
                    for i, pb in enumerate(pbs)
                ),
                msg,
            )
            # TODO: improve bf_id_suffix choices should change when PB is selected, as for XB!


def update_bf_id_suffix_items(ob, context):
    return tuple(
        (
            ("IDI", "Index", "Append index number to multiple ID values"),
            ("IDX", "x", "Append x coordinate to multiple ID values"),
            ("IDY", "y", "Append y coordinate to multiple ID values"),
            ("IDZ", "z", "Append z coordinate to multiple ID values"),
            ("IDXY", "xy", "Append x,y coordinates to multiple ID values"),
            ("IDXZ", "xz", "Append x,z coordinates to multiple ID values"),
            ("IDYZ", "yz", "Append y,z coordinates to multiple ID values"),
            ("IDXYZ", "xyz", "Append x,y,z coordinates to multiple ID values"),
        )[i]
        for i in namelists[ob.bf_namelist_cls].bf_id_suffix_idxs
        or (0, 1, 2, 3, 4, 5, 6, 7)
    )


@subscribe
class OP_ID_suffix(Parameter):
    label = "IDs Suffix"
    description = "Append suffix to multiple ID values"
    bpy_type = Object
    bpy_idname = "bf_id_suffix"
    bpy_prop = EnumProperty
    bpy_other = {"items": update_bf_id_suffix_items}

    def draw(self, context, layout):
        ob = self.element
        if (
            (ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"))
            or (ob.bf_xyz_export and ob.bf_xyz == "VERTICES")
            or ob.bf_pb_export
        ):
            layout.prop(ob, "bf_id_suffix")
        return layout

    def to_fds(self, context):
        return


@subscribe
class OP_SURF_ID(Parameter):
    label = "SURF_ID"
    description = "Reference to SURF"
    fds_label = "SURF_ID"
    bpy_type = Object
    bpy_idname = "active_material"
    bpy_export = "bf_surf_id_export"
    bpy_export_default = True

    @property
    def value(self):
        if self.element.active_material:
            return self.element.active_material.name

    @property
    def exported(self):
        ob = self.element
        return ob.bf_surf_id_export and ob.active_material


@subscribe
class OP_other(POthers):
    bpy_type = Object
    bpy_idname = "bf_others"
    bpy_pg = WM_PG_bf_others
    bpy_ul = WM_UL_bf_others_items


@subscribe
class ON_OBST(Namelist):
    label = "OBST"
    description = "Obstruction"
    enum_id = 1000
    fds_label = "OBST"
    bpy_type = Object
    bpy_export = "bf_export"
    bpy_export_default = True

    param_cls = OP_ID, OP_FYI, OP_SURF_ID, OP_XB, OP_ID_suffix, OP_other
    bf_xb_idxs, bf_id_suffix_idxs = (0, 1, 2, 3), None  # Volume or faces


# Other


@subscribe
class OP_other_namelist(Parameter):
    label = "Label"
    description = "Other namelist label, eg <ABCD>"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_other_namelist"
    bpy_default = "ABCD"
    bpy_other = {"maxlen": 4}

    def check(self, context):
        if not re.match("^[A-Z0-9_]{4}$", self.element.bf_other_namelist):
            raise BFException(self, "Malformed other namelist label")

    def to_fds(self, context):
        return


@subscribe
class ON_other(Namelist):
    label = "Other"
    description = "Other namelist"
    enum_id = 1007
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = (
        OP_other_namelist,
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_XB,
        OP_XYZ,
        OP_PB,
        OP_ID_suffix,
        OP_other,
    )

    @property
    def fds_label(self):
        return self.element.bf_other_namelist


# GEOM


@subscribe
class OP_GEOM_check_quality(Parameter):
    label = "Check Quality While Exporting"
    description = "Check if closed orientable manifold, with no degenerate geometry while exporting"
    bpy_type = Object
    bpy_idname = "bf_geom_check_quality"
    bpy_prop = BoolProperty
    bpy_default = True


@subscribe
class OP_GEOM_protect(Parameter):
    label = "Protect Original"
    description = "Protect original Object geometry while checking quality"
    bpy_type = Object
    bpy_idname = "bf_geom_protect"
    bpy_prop = BoolProperty
    bpy_default = True


@subscribe
class OP_GEOM(Parameter):
    label = "Geometry Parameters"
    description = "Geometry parameters"
    bpy_type = Object

    def draw(self, context, layout):
        pass

    def to_fds(self, context):
        # Check is performed while exporting
        # Get surf_idv, verts and faces
        scale_length = context.scene.unit_settings.scale_length
        check = self.element.bf_geom_check_quality
        fds_surfids, fds_verts, fds_faces, msg = geometry.to_fds.ob_to_geom(
            context, self.element, scale_length, check
        )
        if not fds_faces:
            return None, msg
        # Group by 3 and 4
        verts = [t for t in zip(*[iter(fds_verts)] * 3)]
        faces = [t for t in zip(*[iter(fds_faces)] * 4)]
        # Prepare
        surfids_str = ",".join(("'{}'".format(s) for s in fds_surfids))
        separator1 = "\n      "
        separator2 = "\n            "
        verts_str = separator2.join(
            ("{0[0]:+.6f}, {0[1]:+.6f}, {0[2]:+.6f},".format(v) for v in verts)
        )
        faces_str = separator2.join(
            ("{0[0]},{0[1]},{0[2]}, {0[3]},".format(f) for f in faces)
        )
        return (
            separator1.join(
                (
                    "SURF_ID={}".format(surfids_str),
                    "VERTS={}".format(verts_str),
                    "FACES={}".format(faces_str),
                )
            ),
            msg,
        )


@subscribe
class OP_GEOM_IS_TERRAIN(Parameter):  # FIXME
    label = "IS_TERRAIN"
    description = "Set if it represents a terrain"
    fds_label = "IS_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_is_terrain"


@subscribe
class OP_GEOM_EXTEND_TERRAIN(Parameter):  # FIXME
    label = "EXTEND_TERRAIN"
    description = "Set if this terrain needs extension to fully cover the domain"
    fds_label = "EXTEND_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_extend_terrain"

    @property
    def exported(self):
        ob = self.element
        return ob.bf_geom_is_terrain


@subscribe
class ON_GEOM(Namelist):
    label = "GEOM"
    description = "Geometry"
    enum_id = 1021
    fds_label = "GEOM"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = (
        OP_ID,
        OP_FYI,
        OP_GEOM_check_quality,
        OP_GEOM_IS_TERRAIN,
        OP_GEOM_EXTEND_TERRAIN,
        OP_other,
        OP_GEOM,
    )


# HOLE


@subscribe
class ON_HOLE(Namelist):
    label = "HOLE"
    description = "Obstruction cutout"
    enum_id = 1009
    fds_label = "HOLE"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_XB, OP_ID_suffix, OP_other
    bf_xb_idxs, bf_id_suffix_idxs = (0, 1), None  # Volume


# VENT


@subscribe
class ON_VENT(Namelist):
    label = "VENT"
    description = "Boundary condition patch"
    enum_id = 1010
    fds_label = "VENT"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_SURF_ID, OP_XB, OP_ID_suffix, OP_other
    bf_xb_idxs, bf_id_suffix_idxs = (2, 3), None  # only faces


# DEVC


@subscribe
class OP_DEVC_QUANTITY(PString):
    label = "QUANTITY"
    description = "Output quantity"
    fds_label = "QUANTITY"
    bpy_type = Object
    bpy_idname = "bf_quantity"


@subscribe
class OP_DEVC_SETPOINT(Parameter):
    label = "SETPOINT [~]"
    description = "Value of the device at which its state changes"
    fds_label = "SETPOINT"

    bpy_type = Object
    bpy_idname = "bf_devc_setpoint"
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"step": 10.0, "precision": 3}
    bpy_export = "bf_devc_setpoint_export"
    bpy_export_default = False


@subscribe
class OP_DEVC_INITIAL_STATE(Parameter):
    label = "INITIAL_STATE"
    description = "Set device initial state"
    fds_label = "INITIAL_STATE"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_initial_state"


@subscribe
class OP_DEVC_LATCH(Parameter):
    label = "LATCH"
    description = "Device only changes state once"
    fds_label = "LATCH"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_latch"


@subscribe
class OP_DEVC_PROP_ID(PString):
    label = "PROP_ID"
    description = "Reference to a PROP (Property) line for self properties"
    fds_label = "PROP_ID"
    bpy_type = Object
    bpy_idname = "bf_devc_prop_id"


@subscribe
class ON_DEVC(Namelist):
    label = "DEVC"
    description = "Device"
    enum_id = 1011
    fds_label = "DEVC"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = (
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_DEVC_SETPOINT,
        OP_DEVC_INITIAL_STATE,
        OP_DEVC_LATCH,
        OP_DEVC_PROP_ID,
        OP_XB,
        OP_XYZ,
        OP_ID_suffix,
        OP_other,
    )


# SLCF


@subscribe
class OP_SLCF_VECTOR(Parameter):
    label = "VECTOR"
    description = "Create animated vectors"
    fds_label = "VECTOR"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_vector"


@subscribe
class OP_SLCF_CELL_CENTERED(Parameter):
    label = "CELL_CENTERED"
    description = "Output the actual cell-centered data with no averaging"
    fds_label = "CELL_CENTERED"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_cell_centered"

    def check(self, context):
        if (
            self.element.bf_slcf_cell_centered
            and self.element.bf_slcf_vector
            and not self.element.bf_quantity == "VELOCITY"
        ):
            raise BFException(self, "Cannot set CELL_CENTERED and VECTOR")


@subscribe
class ON_SLCF(Namelist):
    label = "SLCF"
    description = "Slice file"
    enum_id = 1012
    fds_label = "SLCF"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = (
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_SLCF_VECTOR,
        OP_SLCF_CELL_CENTERED,
        OP_XB,
        OP_PB,
        OP_ID_suffix,
        OP_other,
    )
    bf_xb_idxs, bf_id_suffix_idxs = (2,), None  # Faces or planes


# PROF


@subscribe
class ON_PROF(Namelist):
    label = "PROF"
    description = "Wall profile output"
    enum_id = 1013
    fds_label = "PROF"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_DEVC_QUANTITY, OP_XYZ, OP_ID_suffix, OP_other
    # bf_xyz_idxs, bf_id_suffix_idxs = None, None  # FIXME


# MESH


@subscribe
class OP_MESH_IJK(Parameter):
    label = "IJK"
    description = "Cell number in x, y, and z direction"
    fds_label = "IJK"
    bpy_type = Object
    bpy_idname = "bf_mesh_ijk"
    bpy_prop = IntVectorProperty
    bpy_default = (10, 10, 10)
    bpy_other = {"size": 3, "min": 1}
    bpy_export = "bf_mesh_ijk_export"  # FIXME remove
    bpy_export_default = True


@subscribe
class OP_MESH_MPI_PROCESS(Parameter):
    label = "MPI_PROCESS"
    description = "Assigned to given MPI process (Starting from 0.)"
    fds_label = "MPI_PROCESS"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mesh_mpi_process"
    bpy_prop = IntProperty
    bpy_other = {"min": 0}
    bpy_export = "bf_mesh_mpi_process_export"
    bpy_export_default = False


@subscribe
class ON_MESH(Namelist):
    label = "MESH"
    description = "Domain of simulation"
    enum_id = 1014
    fds_label = "MESH"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_MESH_IJK, OP_MESH_MPI_PROCESS, OP_XB, OP_other
    bf_xb_idxs = (0,)  # Only BBOX


# INIT


@subscribe
class ON_INIT(Namelist):
    label = "INIT"
    description = "Initial condition"
    enum_id = 1015
    fds_label = "INIT"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_XB, OP_XYZ, OP_ID_suffix, OP_other
    # bf_xb_idxs, bf_id_suffix_idxs = (1,), None  # FIXME


# ZONE


@subscribe
class ON_ZONE(Namelist):
    label = "ZONE"
    description = "Pressure zone"
    enum_id = 1016
    fds_label = "ZONE"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_XB, OP_ID_suffix, OP_other
    # bf_xb_idxs, bf_id_suffix_idxs = (1,), None  # FIXME


# HVAC


@subscribe
class ON_HVAC(Namelist):
    label = "HVAC"
    description = "HVAC system definition"
    enum_id = 1017
    fds_label = "HVAC"
    bpy_type = Object
    bpy_export = "bf_export"

    param_cls = OP_ID, OP_FYI, OP_XYZ, OP_ID_suffix, OP_other
    # bf_xb_idxs, bf_id_suffix_idxs = (1,), None  # FIXME


# Closing

# Update OP_namelist items

items = [
    (cls.__name__, cls.label, cls.description, cls.enum_id)
    for _, cls in namelists.items()
    if cls.bpy_type == Object
]
items.sort(key=lambda k: k[1])
OP_namelist_cls.bpy_other["items"] = items

# Update MP_namelist items

items = [
    (cls.__name__, cls.label, cls.description, cls.enum_id)
    for _, cls in namelists.items()
    if cls.bpy_type == Material
]
items.sort(key=lambda k: k[1])
MP_namelist_cls.bpy_other["items"] = items


# Extend Blender Object


class BFObject:
    bf_namelist_cls = ""  # Defined as Blender prop
    type = None

    @property
    def bf_namelist(self):
        return namelists[self.bf_namelist_cls](self)

    def to_fds(self, context):
        if self.type == "MESH":
            return self.bf_namelist.to_fds(context)

    @classmethod
    def register(cls):
        Object.bf_namelist = cls.bf_namelist
        Object.to_fds = cls.to_fds

    @classmethod
    def unregister(cls):
        del Object.bf_namelist
        del Object.to_fds


# Extend Blender Material


class BFMaterial:
    bf_namelist_cls = ""  # Defined as Blender prop

    @property
    def bf_namelist(self):
        return namelists[self.bf_namelist_cls](self)

    def to_fds(self, context):
        return self.bf_namelist.to_fds(context)

    @classmethod
    def register(cls):
        Material.bf_namelist = cls.bf_namelist
        Material.to_fds = cls.to_fds

    @classmethod
    def unregister(cls):
        del Material.bf_namelist
        del Material.to_fds


# Extend Blender Scene


class BFScene:
    name = None  # redefined by subclass
    bf_head_export = True  # redefined by subclass

    @property
    def bf_namelists(self):
        return (n for _, n in namelists.items() if n.bpy_type == Scene)

    def to_fds(self, context, full=False):
        # Header
        version = "{0[0]}.{0[1]}.{0[2]}".format(
            sys.modules["blenderfds28x"].bl_info["version"]
        )
        now = time.strftime("%a, %d %b %Y, %H:%M:%S", time.localtime())
        filepath = bpy.data.filepath or "not saved"
        if len(filepath) > 60:
            filepath = "..." + filepath[-57:]
        bodies = [
            f"! Generated by BlenderFDS {version} on Blender {bpy.app.version_string}",
            f"! Scene: <{self.name}>  Date: <{now}>  File: <{filepath}>",
        ]
        bodies.extend(
            n(self).to_fds(context) for n in self.bf_namelists
        )  # my namelists
        # Extend with Materials and Collections
        if full:
            # Materials
            mas = list(bpy.data.materials)
            if mas:
                mas.sort(key=lambda k: k.name)  # alphabetic order by name
                bodies.append("\n! --- Boundary conditions from Blender Materials")
                for ma in mas:
                    bodies.append(ma.to_fds(context))
            # Objects
            bodies.append(context.scene.collection.to_fds(context))
            # Tail
            if self.bf_head_export:
                bodies.append("\n&TAIL /")
        return "\n".join(b for b in bodies if b)  # remove empties

    def to_ge1(self, context):
        return geometry.to_ge1.scene_to_ge1(context, self)

    @classmethod
    def register(cls):
        Scene.bf_namelists = cls.bf_namelists
        Scene.to_fds = cls.to_fds
        Scene.to_ge1 = cls.to_ge1

    @classmethod
    def unregister(cls):
        del Scene.bf_namelists
        del Scene.to_fds
        del Scene.to_ge1


# Extend Blender Collection


class BFCollection:
    name = None  # redefined by subclass
    objects = list()  # redefined by subclass
    children = list()  # redefined by subclass

    def to_fds(self, context):  # FIXME messages and structure
        obs = list(self.objects)
        obs.sort(key=lambda k: k.name)  # alphabetic order by name
        bodies = list()
        if obs:
            bodies.append(
                f"\n! --- Geometric namelists from Blender Collection <{self.name}>"
            )
            bodies.extend(ob.to_fds(context) for ob in obs)
        bodies.extend(child.to_fds(context) for child in self.children)
        return "\n".join(b for b in bodies if b)  # remove empties

    @classmethod
    def register(cls):
        Collection.to_fds = cls.to_fds

    @classmethod
    def unregister(cls):
        del Collection.to_fds


# Register


def register():
    from bpy.utils import register_class

    # Blender classes
    for cls in bl_classes:
        log.info(f"Registering Blender class <{cls.__name__}>")
        register_class(cls)
    # System parameters for tmp obs and file version
    log.info(f"BFDS: registering sys properties")
    Object.bf_is_tmp = BoolProperty(
        name="Is Tmp", description="Set if this Object is tmp", default=False
    )
    Object.bf_has_tmp = BoolProperty(
        name="Has Tmp",
        description="Set if this Object has tmp companions",
        default=False,
    )
    Scene.bf_file_version = IntVectorProperty(
        name="BlenderFDS File Version", size=3, default=(5, 0, 0)
    )  # FIXME
    # params and namelists
    for _, cls in params.items():
        cls.register()
    for _, cls in namelists.items():
        cls.register()
    # Blender Object, Material, and Scene
    BFObject.register()
    BFMaterial.register()
    BFScene.register()
    BFCollection.register()


def unregister():
    from bpy.utils import unregister_class

    # Blender Object, Material, and Scene
    log.info(f"Unregistering sys properties")
    BFObject.unregister()
    BFMaterial.unregister()
    BFScene.unregister()
    BFCollection.unregister()
    # params and namelists
    for _, cls in params.items():
        cls.unregister()
    for _, cls in namelists.items():
        cls.unregister()
    # System parameters for tmp obs and file version
    del Object.bf_is_tmp
    del Object.bf_has_tmp
    del Scene.bf_file_version
    # Blender classes
    for cls in bl_classes:
        log.info(f"Unregistering Blender class <{cls.__name__}>")
        unregister_class(cls)
