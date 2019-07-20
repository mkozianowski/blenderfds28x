"""BlenderFDS, translate Blender object geometry to FDS notation."""

import bpy
from time import time
from . import utils
from .calc_voxels import get_voxels, get_pixels
from .calc_trisurfaces import get_trisurface
from ..lib.exceptions import BFException

DEBUG = False

# ++ to None

# OK
def ob_to_none(context, ob):
    return None, None


# ++ to GEOM


def ob_to_geom(context, ob, check=True) -> "mas, fds_verts, fds_faces, msg":
    """Transform Blender object geometry to GEOM FDS notation. Never send a None."""
    mas, verts, faces = get_trisurface(context, ob, check)
    if faces:
        msg = "{} vertices, {} faces".format(len(verts), len(faces))
    else:
        msg = "No available geometry"
    fds_verts = [coo for vert in verts for coo in vert]
    fds_faces = [i for face in faces for i in face]
    return mas, fds_verts, fds_faces, msg  # TODO add caching of results


# ++ to XB


def ob_to_xbs_voxels(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform ob solid geometry in XBs notation (voxelization). Never send None."""
    t0 = time()
    xbs, voxel_size, timing = get_voxels(context, ob)
    if not xbs:
        return None, "No voxel created"
    scale_length = context.scene.unit_settings.scale_length
    msg = "{0} voxels, resolution {1:.3f} m, in {2:.3f} s".format(
        len(xbs), voxel_size * scale_length, time() - t0
    )
    if DEBUG:
        msg += " (s:{0[0]:.3f} 1f:{0[1]:.3f}, 2g:{0[2]:.3f}, 3g:{0[3]:.3f})".format(
            timing
        )
    return xbs, msg


def ob_to_xbs_pixels(context, ob) -> "((x0,x1,y0,y1,z0,z0,), ...), 'Msg'":
    """Transform ob flat geometry in XBs notation (flat voxelization). Never send None."""
    t0 = time()
    xbs, voxel_size, timing = get_pixels(context, ob)
    if not xbs:
        return (), "No pixel created"
    scale_length = context.scene.unit_settings.scale_length
    msg = "{0} pixels, resolution {1:.3f} m, in {2:.0f} s".format(
        len(xbs), voxel_size * scale_length, time() - t0
    )
    if DEBUG:
        msg += " (s:{0[0]:.0f} 1f:{0[1]:.0f}, 2g:{0[2]:.0f}, 3g:{0[3]:.0f})".format(
            timing
        )
    return xbs, msg


# OK
def ob_to_xbs_bbox(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform ob solid geometry in XBs notation (bounding box). Never send None."""
    return [utils.get_global_bbox(context, ob)], None


# OK
def ob_to_xbs_faces(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform ob faces in XBs notation (faces). Never send None."""
    result = list()
    bm = utils.get_global_bmesh(context, ob)
    # For each face...
    bm.faces.ensure_lookup_table()
    for face in bm.faces:
        verts = face.verts
        xs, ys, zs = tuple(zip(*(v.co for v in verts)))
        x0, x1, y0, y1, z0, z1 = min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)
        deltas = [(x1 - x0, 2), (y1 - y0, 1), (z1 - z0, 0)]
        deltas.sort()
        if deltas[0][1] == 2:
            x1 = x0 = (x0 + x1) / 2.0
        if deltas[0][1] == 1:
            y1 = y0 = (y0 + y1) / 2.0
        if deltas[0][1] == 0:
            z1 = z0 = (z0 + z1) / 2.0
        result.append((x0, x1, y0, y1, z0, z1))
    result.sort()
    bm.free()
    # Return
    msg = len(result) > 1 and "{0} faces".format(len(result)) or None
    return result, msg


# OK
def ob_to_xbs_edges(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform ob faces in XBs notation (faces). Never send None."""
    result = list()
    bm = utils.get_global_bmesh(context, ob)
    # For each edge...
    bm.edges.ensure_lookup_table()
    for edge in bm.edges:
        pt0x, pt0y, pt0z = edge.verts[0].co
        pt1x, pt1y, pt1z = edge.verts[1].co
        result.append((pt0x, pt1x, pt0y, pt1y, pt0z, pt1z))
    result.sort()
    bm.free()
    # Return
    msg = len(result) > 1 and "{0} edges".format(len(result)) or None
    return result, msg


# Caller function (ob.bf_xb)

choice_to_xbs = {
    "NONE": ob_to_none,
    "BBOX": ob_to_xbs_bbox,
    "VOXELS": ob_to_xbs_voxels,
    "FACES": ob_to_xbs_faces,
    "PIXELS": ob_to_xbs_pixels,
    "EDGES": ob_to_xbs_edges,
}

# OK
def ob_to_xbs(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Blender object geometry according to ob.bf_xb to FDS notation. Never send None."""
    # not ob.get("ob_to_xbs_cache") -> precalc not available or modified input conditions
    # if not ob.get("ob_to_xbs_cache"): # ob.is_updated does not work here, checked in the handler
    #     ob["ob_to_xbs_cache"] = choice_to_xbs[ob.bf_xb](context, ob) # Calculate
    # return ob["ob_to_xbs_cache"]  # FIXME cache
    return choice_to_xbs[ob.bf_xb](context, ob)  # Calculate


# ++ to XYZ

# OK
def ob_to_xyzs_vertices(context, ob) -> "((x0,y0,z0,), ...), 'Msg'":
    """Transform ob vertices in XYZs notation. Never send None."""
    result = list()
    bm = utils.get_global_bmesh(context, ob)
    # For each vertex...
    bm.verts.ensure_lookup_table()
    for v in bm.verts:
        pt0x, pt0y, pt0z = v.co
        result.append((pt0x, pt0y, pt0z))
    result.sort()
    bm.free()
    # Return
    msg = len(result) > 1 and "{0} vertices".format(len(result)) or None
    return result, msg


# OK
def ob_to_xyzs_center(context, ob) -> "((x0,y0,z0,), ...), 'Message'":
    """Transform ob center in XYZs notation. Never send None."""
    return [(ob.location[0], ob.location[1], ob.location[2])], None


# Caller function (ob.bf_xyz)

choice_to_xyzs = {
    "NONE": ob_to_none,
    "CENTER": ob_to_xyzs_center,
    "VERTICES": ob_to_xyzs_vertices,
}

# OK
def ob_to_xyzs(context, ob):
    """Transform Blender object geometry according to ob.bf_xyz to FDS notation. Never send None."""
    # # not ob.get("ob_to_xyzs_cache") -> precalc not available or modified input conditions
    # if not ob.get(
    #     "ob_to_xyzs_cache"
    # ):  # ob.is_updated does not work here, checked in the handler
    #     ob["ob_to_xyzs_cache"] = choice_to_xyzs[ob.bf_xyz](context, ob)  # Calculate
    # return ob["ob_to_xyzs_cache"]  # FIXME cache
    return choice_to_xyzs[ob.bf_xyz](context, ob)


# ++ to PB

# OK
def ob_to_pbs_planes(
    context, ob
) -> "(('X',x3,), ('X',x7,), ('Y',y9,), ...), 'Message'":
    """Transform ob faces in PBs notation. Never send None."""
    result = list()
    xbs, msg = ob_to_xbs_faces(context, ob)
    epsilon = 1e-5
    # For each face build a plane...
    for xb in xbs:
        if abs(xb[1] - xb[0]) < epsilon:
            result.append((0, xb[0]))  # PBX is 0
        elif abs(xb[3] - xb[2]) < epsilon:
            result.append((1, xb[2]))  # PBY is 1
        elif abs(xb[5] - xb[4]) < epsilon:
            result.append((2, xb[4]))  # PBZ is 2
        else:
            raise ValueError(
                "BFDS: Building planes impossible, problem in ob_to_xbs_faces."
            )
    result.sort()
    msg = len(result) > 1 and "{0} planes".format(len(result)) or None
    return result, msg


# Caller function (ob.bf_pb)

choice_to_pbs = {"NONE": ob_to_none, "PLANES": ob_to_pbs_planes}

# OK
def ob_to_pbs(context, ob):
    """Transform Blender object geometry according to ob.bf_pb to FDS notation. Never send None."""
    # # not ob.get("ob_to_pbs_cache") -> precalc not available or modified input conditions
    # if not ob.get(
    #     "ob_to_pbs_cache"
    # ):  # ob.is_updated does not work here, checked in the handler
    #     ob["ob_to_pbs_cache"] = choice_to_pbs[ob.bf_pb](context, ob)  # Calculate
    # return ob["ob_to_pbs_cache"] # FIXME cache
    return choice_to_pbs[ob.bf_pb](context, ob)

