"""BlenderFDS, preferences panel"""

import bpy
from bpy.types import AddonPreferences
from bpy.props import BoolProperty, StringProperty, FloatProperty, IntProperty

# Get preference value like this:
# context.preferences.addons["blenderfds28x"].preferences.bf_pref_simplify_ui


class BFPreferences(AddonPreferences):
    bl_idname = "blenderfds28x"

    bf_pref_simplify_ui: BoolProperty(
        name="Simplify UI [Blender restart required]",
        description="Simplify BlenderFDS user interface, Blender restart required",
        default=True,
    )

    bf_quadriflow_filepath: StringProperty(
        name="Quadriflow",
        description="Quadriflow executable filepath (see: github.com/hjwdzh)",
        subtype="FILE_PATH",
    )

    bf_manifold_filepath: StringProperty(
        name="Manifold",
        description="Manifold executable filepath (see: github.com/hjwdzh)",
        subtype="FILE_PATH",
    )

    bf_simplify_filepath: StringProperty(
        name="Simplify",
        description="Simplify executable filepath (see: github.com/hjwdzh)",
        subtype="FILE_PATH",
    )

    def draw(self, context):
        layout = self.layout
        paths = context.preferences.filepaths
        # layout.operator("wm.bf_load_blenderfds_settings") # FIXME BF default settings
        layout.label(text="User Interface:")
        layout.prop(self, "bf_pref_simplify_ui")
        layout.prop(paths, "use_load_ui")
        layout.prop(paths, "use_relative_paths")
        layout.label(text="External executable filepaths:")
        layout.prop(self, "bf_quadriflow_filepath")
        layout.prop(self, "bf_manifold_filepath")
        layout.prop(self, "bf_simplify_filepath")
        return layout


# Register


def register():
    bpy.utils.register_class(BFPreferences)


def unregister():
    bpy.utils.unregister_class(BFPreferences)