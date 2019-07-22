"""BlenderFDS, voxelization algorithms."""

import bpy, bmesh
from time import time
from math import floor, ceil

from mathutils import Matrix

from ..lib.exceptions import BFException
from . import utils

DEBUG = True

# "global" coordinates are absolute coordinate referring to Blender main origin of axes,
# that are directly transformed to FDS coordinates (that refers its coordinates to the
# one and only origin of axes)


def get_voxels(context, ob):
    """Get voxels from object in xbs format."""
    assert ob
    print("BFDS: calc_voxels.get_voxels", ob.name)
    # Check object and init
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise BFException(ob, "Object can not be converted to mesh")
    if not ob.data.vertices:
        raise BFException(ob, "Empty object!")
    voxel_size = _get_voxel_size(context, ob)
    # Work on a full ob copy in world coordinates
    ob_copy = ob.copy()
    ob_copy.data = ob.data.copy()
    ob_copy.data.transform(ob.matrix_world)
    ob_copy.matrix_world = Matrix()
    context.collection.objects.link(ob_copy)
    # Align voxels to global origin and add remesh modifier
    if not ob.bf_xb_center_voxels:
        _align_remesh_to_global_origin(context, ob_copy, voxel_size)
    _add_remesh_mod(context, ob_copy, voxel_size)
    # Get evaluated ob_copy (eg. modifiers applied),
    # already in world coordinates
    dg = context.evaluated_depsgraph_get()
    ob_eval = ob_copy.evaluated_get(dg)
    me_eval = ob_eval.to_mesh()
    # Get bmesh
    bm = bmesh.new()
    bm.from_mesh(me_eval)
    # Clean up
    ob_eval.to_mesh_clear()
    bpy.data.meshes.remove(ob_copy.data)
    # Check
    if len(bm.faces) == 0:  # no faces
        return (), voxel_size, (0.0, 0.0, 0.0, 0.0)
    # Get faces and sort them according to normals
    t1 = time()
    x_faces, y_faces, z_faces = _sort_faces_by_normal(bm)
    # Choose shorter list of faces, relative functions, and parameters
    t2 = time()
    choices = [
        (len(x_faces), _get_boxes_along_x, x_faces, _grow_boxes_along_x, 0),
        (len(y_faces), _get_boxes_along_y, y_faces, _grow_boxes_along_y, 2),
        (len(z_faces), _get_boxes_along_z, z_faces, _grow_boxes_along_z, 4),
    ]
    choices.sort(key=lambda choice: choice[0])
    get_boxes = choices[0][1]  # get boxes by fastest orientation
    faces = choices[0][2]
    grow_boxes_along_first_axis = choices[1][3]  # 1st axis growing direction
    first_sort_by = choices[2][4]
    grow_boxes_along_second_axis = choices[2][3]  # 2nd axis growing direction
    second_sort_by = choices[1][4]
    # For each face find other sides and build boxes data structure
    t3 = time()
    boxes, origin = get_boxes(faces, voxel_size)
    # Join boxes along other axis and return their global coordinates
    t4 = time()
    boxes = grow_boxes_along_first_axis(boxes, first_sort_by)
    t5 = time()
    boxes = grow_boxes_along_second_axis(boxes, second_sort_by)
    t6 = time()
    # Clean up
    bm.free()
    # Return with timing: sort, 1b, 2g, 3g
    xbs = list(_get_box_xbs(boxes, origin, voxel_size))
    return xbs, voxel_size, (t2 - t1, t4 - t3, t5 - t4, t6 - t5)


def _sort_faces_by_normal(bm):
    """Sort bmesh faces according to normal."""
    x_faces, y_faces, z_faces = list(), list(), list()
    for face in bm.faces:
        normal = face.normal
        if abs(normal[0]) > 0.9:
            x_faces.append(face)  # face is normal to x axis
        elif abs(normal[1]) > 0.9:
            y_faces.append(face)  # ... to y axis
        elif abs(normal[2]) > 0.9:
            z_faces.append(face)  # ... to z axis
        else:
            raise ValueError("BFDS: abnormal face")
    if len(x_faces) < 2 or len(y_faces) < 2 or len(z_faces) < 2:
        raise ValueError("BFDS: not enough faces")
    return x_faces, y_faces, z_faces


# When appling a remesh modifier to a Blender Object in BLOCKS mode,
# the object max dimension is scaled up and divided in
# (2 ** octree_depth voxels - 1) cubic voxels
# Example: dimension = 4.2, voxel_size = 0.2,
# octree_depth = 5, number of voxels = 2^5-1 = 31,
# scale = 3/4 = 0.75
# The next function reverses the procedures and calculate octree_depth
# and scale that generate the desired voxel_size.


def _init_remesh_mod(context, ob, voxel_size) -> "octree_depth, scale":
    """Calc remesh modifier parameters from voxel_size."""
    dimension, octree_depth = max(ob.dimensions), 0.0
    while True:
        octree_depth += 1.0
        scale = dimension / voxel_size / 2 ** octree_depth
        if 0.010 < scale < 0.990:
            break
        if octree_depth > 9:
            raise BFException(
                ob, "Object too large for its voxel size, split in parts."
            )
    return octree_depth, scale


def _add_remesh_mod(context, ob, voxel_size) -> "modifier":
    """Add new blocks remesh modifier."""
    octree_depth, scale = _init_remesh_mod(context, ob, voxel_size)
    mo = ob.modifiers.new("remesh_tmp", "REMESH")
    mo.mode, mo.use_remove_disconnected = "BLOCKS", False
    mo.octree_depth, mo.scale = octree_depth, scale
    return mo


def _get_voxel_size(context, ob) -> "voxel_size":
    """Get voxel_size of an object."""
    if ob.bf_xb_custom_voxel:
        return ob.bf_xb_voxel_size
    else:
        return context.scene.bf_default_voxel_size


# When appling a remesh modifier to a Blender Object, the octree is aligned with
# the max dimension of the object. By inserting some loose vertices to the
# temporary object, we can align the voxelization to FDS global origin


def _align_remesh_to_global_origin(context, ob, voxel_size):
    """Modify object mesh for remesh voxel alignment to global origin."""
    bb = utils.get_bbox(ob)  # the object is already global
    # Calc new bbox (in Blender units)
    #      +---+ pv1
    #      |   |
    #  pv0 +---+
    pv0 = (
        floor(bb[0] / voxel_size) - 1,  # at least one voxel away from obj
        floor(bb[2] / voxel_size) - 1,
        floor(bb[4] / voxel_size) - 1,
    )
    pv1 = (
        ceil(bb[1] / voxel_size) + 1,  # at least one voxel away from obj
        ceil(bb[3] / voxel_size) + 1,
        ceil(bb[5] / voxel_size) + 1,
    )
    pv1 = (
        pv0[0] + (pv1[0] - pv0[0]) // 2 * 2 + 1,  # at least one voxel away from obj
        pv0[1] + (pv1[1] - pv0[1]) // 2 * 2 + 1,  # and odd number of voxels
        pv0[2] + (pv1[2] - pv0[2]) // 2 * 2 + 1,
    )
    bb = (
        pv0[0] * voxel_size,
        pv1[0] * voxel_size,
        pv0[1] * voxel_size,
        pv1[1] * voxel_size,
        pv0[2] * voxel_size,
        pv1[2] * voxel_size,
    )
    verts = (
        (bb[0], bb[2], bb[4]),
        (bb[0], bb[2], bb[5]),
        (bb[0], bb[3], bb[4]),
        (bb[0], bb[3], bb[5]),
        (bb[1], bb[2], bb[4]),
        (bb[1], bb[2], bb[5]),
        (bb[1], bb[3], bb[4]),
        (bb[1], bb[3], bb[5]),
    )
    utils.insert_verts_into_mesh(ob.data, verts)


# The following functions transform remesh modifier faces into boxes,
# by raytracing along the requested axis. Each face is transformed into
# integer coordinates according to a local origin (the first face center).
# The faces are piled up, sorted, and transformed into solids:
# Eg.: z axis --> pile 0|==solid==1| void 2|==solid==3| void ...
# If solid is manifold, len(izs) is an even number:
# go into solid at izs[0], get at last out of it at izs[-1].
# In fact this piles can be easily transformed in boxes:
# (ix0, ix1, iy0, iy1, iz0, iz1)
# boxes are very alike XBs, but in integer coordinates.

# For example, with y faces:
#  y ^
#    |     B
#  3 +---+=x=+---+ F = (.5, 0): face origin (integer coordinates)
#    |   H   H   | face A = (1, 0, 0)
#  2 +---+---+---+ face B = (1, 3, 0)
#    |   H   H   | O = (0, 0): box origin (integer coordinate)
#  1 +---+---+---+ box AB = (1, 2, 0, 3, 0, 0)
#    |   H   H   |
#  0 +-F-+=x=+---+->
#    0   1 A 2   3 x


def _get_face_center(face):
    """Get bmesh face center point."""
    xs, ys, zs = zip(*(v.co for v in face.verts))
    return sum(xs) / len(xs), sum(ys) / len(ys), sum(zs) / len(zs)


def _get_boxes_along_x(faces, voxel_size) -> "boxes, origin":
    """Get minimal boxes from faces by raytracing along x axis."""
    DEBUG and print("BFDS: _get_boxes_along_x")
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0], f_origin[1] - hvs, f_origin[2] - hvs)
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(iy, iz)].append(ix)
        except KeyError:
            piles[(iy, iz)] = [ix]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (iy, iz), ixs in piles.items():
        ixs.sort()  # sort in +x direction
        while ixs:
            ix1, ix0 = ixs.pop(), ixs.pop()  # pop solid volumes from top to bottom
            boxes.append([ix0, ix1, iy, iy + 1, iz, iz + 1])
    return boxes, origin


def _get_boxes_along_y(faces, voxel_size) -> "boxes, origin":
    """Get minimal boxes from faces by raytracing along y axis."""
    DEBUG and print("BFDS: _get_boxes_along_y")
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0] - hvs, f_origin[1], f_origin[2] - hvs)
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(iz, ix)].append(iy)
        except KeyError:
            piles[(iz, ix)] = [iy]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (iz, ix), iys in piles.items():
        iys.sort()  # sort in +y direction
        while iys:
            iy1, iy0 = iys.pop(), iys.pop()  # pop solid volumes from top to bottom
            boxes.append([ix, ix + 1, iy0, iy1, iz, iz + 1])
    return boxes, origin


def _get_boxes_along_z(faces, voxel_size) -> "boxes, origin":
    """Get minimal boxes from faces by raytracing along z axis."""
    DEBUG and print("BFDS: _get_boxes_along_z")
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0] - hvs, f_origin[1] - hvs, f_origin[2])
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(ix, iy)].append(iz)
        except KeyError:
            piles[(ix, iy)] = [iz]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (ix, iy), izs in piles.items():
        izs.sort()  # sort in +z direction
        while izs:
            iz1, iz0 = izs.pop(), izs.pop()  # pop solid volumes from top to bottom
            boxes.append([ix, ix + 1, iy, iy + 1, iz0, iz1])
    return boxes, origin


# The following functions reduce the number of boxes in xbs format,
# used to describe the geometry, by merging them


def _grow_boxes_along_x(boxes, sort_by):
    """Grow boxes by merging neighbours along x axis."""
    DEBUG and print("BFDS: _grow_boxes_along_x")
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[0]))
    # Grow boxes in -x direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iz0, iz1, iy0, iy1, and touching abox ix1 with box ix0
        if (
            abox[4] == box[4]
            and abox[5] == box[5]
            and abox[2] == box[2]
            and abox[3] == box[3]
            and abox[1] == box[0]
        ):
            box[0] = abox[0]  # grow box along -x
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


def _grow_boxes_along_y(boxes, sort_by):
    """Grow boxes by merging neighbours along y axis."""
    DEBUG and print("BFDS: _grow_boxes_along_y")
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[2]))
    # Grow boxes in -y direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iz0, iz1, ix0, ix1, and touching abox iy1 with box iy0
        if (
            abox[4] == box[4]
            and abox[5] == box[5]
            and abox[0] == box[0]
            and abox[1] == box[1]
            and abox[3] == box[2]
        ):
            box[2] = abox[2]  # grow box along -y
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


def _grow_boxes_along_z(boxes, sort_by):
    """Grow boxes by merging neighbours along z axis."""
    DEBUG and print("BFDS: _grow_boxes_along_z")
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[4]))
    # Grow boxes in -z direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iy0, iy1, ix0, ix1, and touching abox iz1 with box iz0
        if (
            abox[2] == box[2]
            and abox[3] == box[3]
            and abox[0] == box[0]
            and abox[1] == box[1]
            and abox[5] == box[4]
        ):
            box[4] = abox[4]  # grow box along -z
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


# Transform boxes in integer coordinates, back to global coordinates


def _get_box_xbs(boxes, origin, voxel_size) -> "xbs":
    """Transform boxes to xbs in global coordinates."""
    epsilon = 1e-5
    return (
        (
            origin[0] + box[0] * voxel_size - epsilon,
            origin[0] + box[1] * voxel_size + epsilon,
            origin[1] + box[2] * voxel_size - epsilon,
            origin[1] + box[3] * voxel_size + epsilon,
            origin[2] + box[4] * voxel_size - epsilon,
            origin[2] + box[5] * voxel_size + epsilon,
        )
        for box in boxes
    )


# Pixelization


def get_pixels(context, ob):
    """Get pixels from flat object in xbs format."""
    assert ob
    print("BFDS: calc_voxels.get_voxels", ob.name)
    # Check object and init
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise BFException(ob, "Object can not be converted to mesh")
    if not ob.data.vertices:
        raise BFException(ob, "Empty object!")
    voxel_size = _get_voxel_size(context, ob)
    flat_axis = _get_flat_axis(ob, voxel_size)
    # Work on a full ob copy in world coordinates
    ob_copy = ob.copy()
    ob_copy.data = ob.data.copy()
    ob_copy.data.transform(ob.matrix_world)
    ob_copy.matrix_world = Matrix()
    context.collection.objects.link(ob_copy)
    # Check how flat it is
    if ob_copy.dimensions[flat_axis] > voxel_size / 3.0:
        bpy.data.meshes.remove(ob_copy.data)
        raise BFException(ob, "Object is not flat enough.")
    # Get origin for flat xbs
    bbox = utils.get_bbox(ob_copy)
    flat_origin = (
        (bbox[1] + bbox[0]) / 2.0,
        (bbox[3] + bbox[2]) / 2.0,
        (bbox[5] + bbox[4]) / 2.0,
    )
    # Add solidify modifier
    _add_solidify_mod(context, ob_copy, voxel_size)
    # Voxelize
    xbs, voxel_size, ts = get_voxels(context, ob_copy)
    # Clean up
    bpy.data.meshes.remove(ob_copy.data)
    # Flatten the solidified object
    choice = (_x_flatten_xbs, _y_flatten_xbs, _z_flatten_xbs)[flat_axis]
    xbs = choice(xbs, flat_origin)
    return xbs, voxel_size, ts


def get_pixels_old(context, ob):  # FIXME
    """Get pixels from flat object in xbs format."""
    assert ob
    print("BFDS: calc_voxels.get_voxels", ob.name)
    voxel_size = _get_voxel_size(context, ob)
    flat_axis = _get_flat_axis(ob, voxel_size)
    # Create new object, and link it. Then prepare it for voxelization
    ob_tmp = utils.get_object_copy(context, ob, suffix="_pix_tmp")
    ob_tmp.bf_xb_voxel_size = voxel_size
    ob_tmp.bf_xb_custom_voxel = True
    ob_tmp.bf_xb_center_voxels = ob.bf_xb_center_voxels
    # Check how flat it is
    if ob_tmp.dimensions[flat_axis] > voxel_size / 3.0:
        utils.rm_object(ob_tmp)
        raise BFException(ob, "Object is not flat enough.")
    # Get origin for flat xbs
    bbox = utils.get_bbox(ob_tmp)
    flat_origin = (
        (bbox[1] + bbox[0]) / 2.0,
        (bbox[3] + bbox[2]) / 2.0,
        (bbox[5] + bbox[4]) / 2.0,
    )
    # Add solidify modifier, apply and remove it
    _add_solidify_mod(context, ob_tmp, voxel_size)
    utils.apply_object_modifiers(context, ob_tmp)
    # Voxelize
    xbs, voxel_size, ts = get_voxels(context, ob_tmp)
    # Flatten the solidified object
    choice = (_x_flatten_xbs, _y_flatten_xbs, _z_flatten_xbs)[flat_axis]
    xbs = choice(xbs, flat_origin)
    # Clean and return
    utils.rm_object(ob_tmp)
    return xbs, voxel_size, ts


def _add_solidify_mod(context, ob, voxel_size) -> "modifier":
    """Add new solidify modifier."""
    mo = ob.modifiers.new("solidify_tmp", "SOLIDIFY")
    mo.thickness = voxel_size
    mo.offset = 0.0  # centered
    return mo


def _get_flat_axis(ob, voxel_size):
    """Get object flat axis."""
    dimensions = ob.dimensions
    choices = [
        (dimensions[0], 0),  # object faces are normal to x axis
        (dimensions[1], 1),  # ... to y axis
        (dimensions[2], 2),  # ... to z axis
    ]
    choices.sort(key=lambda k: k[0])  # sort by dimension
    return choices[0][1]


def _x_flatten_xbs(xbs, flat_origin) -> "[(l0, l0, y0, y1, z0, z1), ...]":
    """Flatten voxels to obtain pixels (normal to x axis) at flat_origin height."""
    return [[flat_origin[0], flat_origin[0], xb[2], xb[3], xb[4], xb[5]] for xb in xbs]


def _y_flatten_xbs(xbs, flat_origin) -> "[(x0, x1, l0, l0, z0, z1), ...]":
    """Flatten voxels to obtain pixels (normal to y axis) at flat_origin height."""
    return [[xb[0], xb[1], flat_origin[1], flat_origin[1], xb[4], xb[5]] for xb in xbs]


def _z_flatten_xbs(xbs, flat_origin) -> "[(x0, x1, y0, y1, l0, l0), ...]":
    """Flatten voxels to obtain pixels (normal to z axis) at flat_origin height."""
    return [[xb[0], xb[1], xb[2], xb[3], flat_origin[2], flat_origin[2]] for xb in xbs]
