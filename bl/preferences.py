"""BlenderFDS, preferences panel"""

import bpy
import logging

from bpy.types import AddonPreferences
from bpy.props import (
    BoolProperty,
    StringProperty,
    FloatProperty,
    IntProperty,
    EnumProperty,
)

log = logging.getLogger(__name__)

# Get preference value like this:
# prefs = context.preferences.addons[__package__.split(".")[0]].preferences
# prefs.bf_pref_simplify_ui


# Preferences


class BFPreferences(AddonPreferences):
    bl_idname = __package__.split(".")[0]

    bf_pref_simplify_ui: BoolProperty(
        name="Simplify UI [Blender restart required]",
        description="Simplify BlenderFDS user interface, Blender restart required",
        default=True,
    )

    bf_pref_appearance: BoolProperty(
        name="Set Default Appearance",
        description="Automatically set default appearance to Blender Scenes, Objects, Materials,\ndepending on FDS namelist and parameters",
        default=True,
    )

    def update_loglevel(self, context):
        log.setLevel(self.bf_loglevel)

    bf_loglevel: EnumProperty(
        name="Log Level",
        description="Select the log level",
        items=[
            ("DEBUG", "Debug", ""),
            ("INFO", "Info", ""),
            ("WARNING", "Warning", ""),
            ("ERROR", "Error", ""),
            ("CRITICAL", "Critical", ""),
        ],
        update=update_loglevel,
        default="DEBUG",
    )

    def draw(self, context):
        paths = context.preferences.filepaths
        layout = self.layout
        box = layout.box()
        box.label(text="User Interface")
        box.operator("wm.bf_load_blenderfds_settings")
        box.prop(self, "bf_pref_simplify_ui")
        box.prop(self, "bf_pref_appearance")
        box.prop(paths, "use_load_ui")
        box.prop(paths, "use_relative_paths")
        box.prop(self, "bf_loglevel")
        return layout


# Register


def register():
    bpy.utils.register_class(BFPreferences)


def unregister():
    bpy.utils.unregister_class(BFPreferences)
