"""BlenderFDS, temporary object management."""

import bpy

def restore_all(context): # TODO sposta e elimina file
    """Restore all original obs, delete all tmp objects, delete all cached geometry."""
    if context.mode != 'OBJECT': bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
    sc = context.scene
    # For all objects in this scene
    for ob in sc.objects:
        # Unlink and delete temporary object
        if ob.bf_is_tmp:
            bpy.data.objects.remove(ob, do_unlink=True)
            continue
        # Restore original object and delete cached geometry
        if ob.bf_has_tmp: ob.bf_has_tmp, ob.hide = False, False
        ob["ob_to_xbs_cache"] = False
        ob["ob_to_xyzs_cache"] = False
        ob["ob_to_pbs_cache"] = False
