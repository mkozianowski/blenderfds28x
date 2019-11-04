import bpy, logging
from bpy.app.handlers import persistent, load_post, save_pre, depsgraph_update_post
from bpy.types import Object

from .. import geometry
from .. import config

log = logging.getLogger(__name__)

# Handlers


@persistent
def _load_post(self):  # Beware: self is None
    # Check file format version
    bf_file_version = tuple(bpy.data.scenes[0].bf_file_version)
    if not bf_file_version or bf_file_version < config.supported_file_version:  # older
        msg = "Check your old input data!"
        description = "This file was created on a previous BlenderFDS version.\nAutomatic data conversion is not supported."
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT", msg=msg, description=description, type="ERROR"
        )
    elif bf_file_version > config.supported_file_version:  # newer
        msg = "Install latest BlenderFDS!"
        description = "This file was created on a newer BlenderFDS version.\nNew features may not be supported."
        bpy.ops.wm.bf_dialog(
            "INVOKE_DEFAULT", msg=msg, description=description, type="ERROR"
        )
    # Init FDS default materials
    for k, v in config.default_mas.items():
        if not bpy.data.materials.get(k):  # check existance
            ma = bpy.data.materials.new(k)
            ma.bf_surf_export = True
            ma.diffuse_color = v[0]
            ma.use_fake_user = True
    # Set default scene appearance
    for sc in bpy.data.scenes:
        sc.set_default_appearance(context=None)


@persistent
def _save_pre(self):  # Beware: self is None
    # Remove tmp objecys
    geometry.utils.rm_tmp_objects(bpy.context)
    # Set file format version
    for sc in bpy.data.scenes:
        sc.bf_file_version = config.supported_file_version


@persistent
def _depsgraph_update_post(scene):  # FIXME test, was crashing
    # Detect object change and erase cached geometry
    for update in bpy.context.view_layer.depsgraph.updates:
        ob = update.id.original
        if (
            isinstance(ob, Object)
            and ob.type in {"MESH", "CURVE", "SURFACE", "FONT", "META"}
            and (update.is_updated_geometry or update.is_updated_transform)
        ):
            log.debug(f"Remove <{ob.name}> caches")
            ob["ob_to_geom_cache"] = None
            ob["ob_to_xbs_cache"] = None
            ob["ob_to_xyzs_cache"] = None
            ob["ob_to_pbs_cache"] = None


# Register


def register():
    log.debug(f"Registering handlers")
    load_post.append(_load_post)
    save_pre.append(_save_pre)
    depsgraph_update_post.append(_depsgraph_update_post)  # FIXME


def unregister():
    log.debug(f"Unregistering handlers")
    load_post.remove(_load_post)
    save_pre.remove(_save_pre)
    depsgraph_update_post.remove(_depsgraph_update_post)  # FIXME
