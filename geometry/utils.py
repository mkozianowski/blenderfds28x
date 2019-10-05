"""BlenderFDS, geometric utilities."""

import bpy, bmesh

from ..types import BFException


# Working on Blender objects


def get_object_bmesh(context, ob, world=False) -> "BMesh":
    """Return evaluated object bmesh."""
    if context.object:
        bpy.ops.object.mode_set(mode="OBJECT")  # actualize
    bm = bmesh.new()
    depsgraph = context.evaluated_depsgraph_get()
    bm.from_object(ob, depsgraph=depsgraph, deform=True, cage=False, face_normals=True)
    if world:
        bm.transform(ob.matrix_world)  # world coo
    return bm


def get_tmp_object(context, ob, name="tmp"):
    """Get a new tmp Object from ob."""
    # Create new tmp Object
    co_tmp = context.collection
    me_tmp = bpy.data.meshes.new(name)
    ob_tmp = bpy.data.objects.new(name, me_tmp)
    ob_tmp.bf_is_tmp = True
    co_tmp.objects.link(ob_tmp)
    # Set original
    ob.bf_has_tmp = True
    ob.hide_set(True)
    return ob_tmp


def rm_tmp_objects(context):
    mes = bpy.data.meshes
    for ob in context.scene.objects:
        if ob.bf_is_tmp:
            mes.remove(
                ob.data, do_unlink=True
            )  # best way to remove an Object wo mem leaks
        elif ob.bf_has_tmp:
            ob.bf_has_tmp = False
            ob.hide_set(False)
            ob.select_set(True)


# Working on Blender materials


def get_new_material(context, name) -> "Material":
    """Create new material, named name."""
    return bpy.data.materials.new(name)


def get_material_by_name(context, name) -> "Material or None":
    """Get a material by name and return it"""
    if name and name in bpy.data.materials:
        return bpy.data.materials[name]


def get_material(context, name) -> "Material or None":
    """Get an existing material by name or create a new one, and return it"""
    if name and name in bpy.data.materials:
        return bpy.data.materials[name]
    return bpy.data.materials.new(name)


# Working on bounding box and size


def get_bbox_xbs(context, ob, scale_length, world=False) -> "x0, x1, y0, y1, z0, z1":
    """Get object’s bounding box in xbs format."""
    if world:
        bm = get_object_bmesh(context, ob, world=True)
        bm.verts.ensure_lookup_table()
        if not bm.verts:
            raise BFException(ob, "No exported geometry!")
        xs, ys, zs = tuple(zip(*(v.co for v in bm.verts)))
        bm.free()
        return (
            min(xs) * scale_length,
            max(xs) * scale_length,
            min(ys) * scale_length,
            max(ys) * scale_length,
            min(zs) * scale_length,
            max(zs) * scale_length,
        )
    else:
        bb = ob.bound_box  # needs updated view_layer
        return (
            bb[0][0] * scale_length,
            bb[6][0] * scale_length,
            bb[0][1] * scale_length,
            bb[6][1] * scale_length,
            bb[0][2] * scale_length,
            bb[6][2] * scale_length,
        )

