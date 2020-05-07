"""!
BlenderFDS, import/export menu panel
"""

import os

import bpy, logging
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty, FloatProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper

from .. import utils
from ..types import BFException, FDSCase

log = logging.getLogger(__name__)


# Collections

bl_classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collection.
    @param cls: the class to subscribe.
    @return the class subscribed.
    """
    bl_classes.append(cls)
    return cls


# Import menus


@subscribe
class ImportFDS(Operator, ImportHelper):
    """!
    Import FDS case file to a Scene.
    """

    bl_idname = "import_scene.fds"
    bl_label = "Import FDS"
    bl_options = {"UNDO"}

    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})
    new_scene: BoolProperty(name="Into New Scene", default=True)

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return True if operator can be called, False otherwise.
        """
        return context.scene is not None

    def execute(self, context):
        """!
        Execute the operator.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @return return
        - "RUNNING_MODAL" keep the operator running with blender.
        - "CANCELLED" when no action has been taken, operator exits.
        - "FINISHED" when the operator is complete, operator exits.
        - "PASS_THROUGH" do nothing and pass the event on.
        - "INTERFACE" handled but not executed (popup menus).
        """
        # Init
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        # Read and parse
        fds_case = FDSCase()
        try:
            fds_case.from_fds(utils.read_from_file(self.filepath))
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Read or parse error: {str(err)}")
            return {"CANCELLED"}
        # Current or new Scene
        if self.new_scene:
            sc = bpy.data.scenes.new("Imported")
        else:
            sc = context.scene
        # Import
        try:
            sc.from_fds(context, fds_case=fds_case)
        except BFException as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Import error: {str(err)}")
            return {"CANCELLED"}
        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, "FDS case imported")
        return {"FINISHED"}


def menu_func_import_FDS(self, context):
    """!
    Function to import FDS into a new scene.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    """
    self.layout.operator(
        "import_scene.fds", text="FDS (.fds) into New Scene"
    ).new_scene = True


def menu_func_import_snippet_FDS(self, context):
    """!
    Function to import FDS into the current scene.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    """
    self.layout.operator(
        "import_scene.fds", text="FDS (.fds) into Current Scene"
    ).new_scene = False


# Export menu


@subscribe
class ExportFDS(Operator, ExportHelper):
    """!
    Export current Scene to FDS case file.
    """

    bl_idname = "export_scene.fds"
    bl_label = "Export FDS"
    bl_description = "Export Blender Scenes as FDS case files"

    # Inherited from ExportHelper
    filename_ext = ".fds"
    filter_glob: StringProperty(default="*.fds", options={"HIDDEN"})
    directory: StringProperty(
        name="Directory",
        description="Directory used for exporting the file",
        maxlen=1024,
        subtype="DIR_PATH",
        options={"HIDDEN"},
    )

    # Export all scenes
    all_scenes: BoolProperty(name="All Scenes", default=False)

    @classmethod
    def poll(cls, context):
        return context.scene is not None  # at least one available scene

    def _write_fds_file(self, context, sc, filepath):
        w = context.window_manager.windows[0]
        # Write .fds file
        log.debug(f"Exporting Blender Scene <{sc.name}>")
        w.cursor_modal_set("WAIT")
        try:
            utils.write_to_file(filepath, sc.to_fds(context=context, full=True))
        except BFException as err:
            self.report({"ERROR"}, f"Error assembling FDS file:\n<{str(err)}>")
            return {"CANCELLED"}
        except IOError:
            self.report({"ERROR"}, f"Filepath not writable:\n<{filepath}>")
            return {"CANCELLED"}
        except Exception as err:
            self.report({"ERROR"}, f"Unexpected error:\n<{str(err)}>")
            return {"CANCELLED"}
        finally:
            w.cursor_modal_restore()
        if sc.bf_dump_render_file:
            # Write .ge1 file
            w.cursor_modal_set("WAIT")
            try:
                filepath = filepath[:-4] + ".ge1"
                utils.write_to_file(filepath, sc.to_ge1(context=context))
            except BFException as err:
                self.report({"ERROR"}, f"Error assembling GE1 file:\n<{str(err)}>")
                return {"CANCELLED"}
            except IOError:
                self.report({"ERROR"}, f"Filepath not writable:\n<{filepath}>")
                return {"CANCELLED"}
            except Exception as err:
                self.report({"ERROR"}, f"Unexpected error:\n<{str(err)}>")
                return {"CANCELLED"}
            finally:
                w.cursor_modal_restore()
        self.report({"INFO"}, f"FDS exporting ok")
        return {"FINISHED"}

    def invoke(self, context, event):
        sc = context.scene
        basename = f"{bpy.path.clean_name(sc.name)}.fds"
        directory = bpy.path.abspath(
            sc.bf_config_directory or os.path.dirname(bpy.data.filepath)
        )
        self.filepath = "/".join((directory, basename))
        return super().invoke(context, event)  # open dialog

    def execute(self, context):
        if self.all_scenes:
            # Export all scenes to configure dir or chosen dir
            for sc in bpy.data.scenes:
                basename = f"{bpy.path.clean_name(sc.name)}.fds"
                directory = bpy.path.abspath(sc.bf_config_directory or self.directory)
                filepath = "/".join((directory, basename))
                res = self._write_fds_file(context=context, sc=sc, filepath=filepath)
                if res != {"FINISHED"}:  # break if any goes wrong
                    break
            return res
        else:
            # Export current scene to chosen dir
            return self._write_fds_file(
                context=context, sc=context.scene, filepath=self.filepath
            )


def menu_func_export_to_fds(self, context):
    """!
    Export function for current Scene to FDS case file.
    """
    self.layout.operator(ExportFDS.bl_idname, text="FDS (.fds)")


# Register


def register():
    """!
    Load the Python classes and functions to Blender.
    """
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import_snippet_FDS)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export_to_fds)


def unregister():
    """!
    Unload the Python classes and functions from Blender.
    """
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_FDS)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import_snippet_FDS)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export_to_fds)
