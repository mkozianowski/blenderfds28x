import bpy
from bpy.app.handlers import persistent, load_post, save_pre, depsgraph_update_post
from bpy.types import Object

from .. import geometry

# Handlers


@persistent
def _load_post(self):  # Beware: self is None
    context = bpy.context
    # Check file format version FIXME
    # check_file_version(context)
    # Init FDS default materials
    # if not fds.surf.has_predefined(): bpy.ops.material.bf_set_predefined()
    # Set default scene appearance
    # for scene in bpy.data.scenes: scene.set_default_appearance(context=None)


@persistent
def _save_pre(self):  # Beware: self is None
    geometry.utils.rm_tmp_objects(bpy.context)
    # Set file format version
    # set_file_version(bpy.context) FIXME


@persistent
def _depsgraph_update_post(scene):
    # Detect object change and erase cached geometry
    for update in bpy.context.view_layer.depsgraph.updates:
        ob = update.id.original
        if (
            isinstance(ob, Object)
            and ob.type in {"MESH", "CURVE", "SURFACE", "FONT", "META"}
            and (update.is_updated_geometry or update.is_updated_transform)
        ):
            ob["ob_to_geom_cache"] = False
            ob["ob_to_xbs_cache"] = False
            ob["ob_to_xyzs_cache"] = False
            ob["ob_to_pbs_cache"] = False
            print("BFDS: _depsgraph_update_post updated:", ob.name)  # FIXME


# Register


def register():
    print("BFDS: Registering handlers")
    load_post.append(_load_post)
    save_pre.append(_save_pre)
    depsgraph_update_post.append(_depsgraph_update_post)


def unregister():
    print("BFDS: Unregistering handlers")
    load_post.remove(_load_post)
    save_pre.remove(_save_pre)
    depsgraph_update_post.remove(_depsgraph_update_post)
