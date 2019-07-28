"""BlenderFDS, translate Blender object geometry to FDS notation."""

import bpy
from time import time
from . import utils
from . import calc_voxels
from . import calc_trisurfaces
from ..lib.exceptions import BFException


# to GEOM in Blender units

# FIXME caching is deleted when created
def ob_to_geom(context, ob, check=True) -> "mas, fds_verts, fds_faces, 'Msg'":
    """Transform Object geometry to FDS mas, verts, faces notation."""
    if not ob.get("ob_to_geom_cache"):
        t0 = time()
        print("BFDS: ob_to_geom recalc:", ob.name)
        mas, verts, faces = calc_trisurfaces.get_trisurface(context, ob, check)
        fds_verts = [coo for vert in verts for coo in vert]
        fds_faces = [i for face in faces for i in face]
        dt = time() - t0
        msg = f"GEOM: {len(fds_verts)} vertices, {len(fds_faces)} faces, in {dt:.3f} s"
        ob["ob_to_geom_cache"] = mas, fds_verts, fds_faces, msg
    return ob["ob_to_geom_cache"]


# to XB in Blender units


def _ob_to_xbs_voxels(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Object solid geometry to xbs notation (voxelization)."""
    t0 = time()
    xbs, voxel_size = calc_voxels.get_voxels(context, ob)
    res = voxel_size * context.scene.unit_settings.scale_length
    dt = time() - t0
    msg = f"XB: {len(xbs)} voxels, resolution {res:.3f} m, in {dt:.3f} s"
    return xbs, msg


def _ob_to_xbs_pixels(context, ob) -> "((x0,x1,y0,y1,z0,z0,), ...), 'Msg'":
    """Transform Object flat geometry to xbs notation (flat voxelization)."""
    t0 = time()
    xbs, voxel_size = calc_voxels.get_pixels(context, ob)
    res = voxel_size * context.scene.unit_settings.scale_length
    dt = time() - t0
    msg = f"XB: {len(xbs)} pixels, resolution {res:.3f} m, in {dt:.3f} s"
    return xbs, msg


def _ob_to_xbs_bbox(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Object solid geometry to xbs notation (bounding box)."""
    xbs = list((utils.get_global_bbox(context, ob),))
    msg = str()
    return xbs, msg


def _ob_to_xbs_faces(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Object flat faces to xbs notation (faces)."""
    xbs = list()
    bm = utils.get_global_bmesh(context, ob)
    bm.faces.ensure_lookup_table()
    for face in bm.faces:
        verts = face.verts
        xs, ys, zs = tuple(zip(*(v.co for v in verts)))
        x0, x1, y0, y1, z0, z1 = (min(xs), max(xs), min(ys), max(ys), min(zs), max(zs))
        deltas = [(x1 - x0, 2), (y1 - y0, 1), (z1 - z0, 0)]
        deltas.sort()
        if deltas[0][1] == 2:
            x1 = x0 = (x0 + x1) / 2.0
        if deltas[0][1] == 1:
            y1 = y0 = (y0 + y1) / 2.0
        if deltas[0][1] == 0:
            z1 = z0 = (z0 + z1) / 2.0
        xbs.append((x0, x1, y0, y1, z0, z1))
    bm.free()
    xbs.sort()
    if not xbs:
        raise BFException(ob, "XB: No exported faces!")
    msg = f"XB: {len(xbs)} faces"
    return xbs, msg


def _ob_to_xbs_edges(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Object edges in XBs notation (edges)."""
    xbs = list()
    bm = utils.get_global_bmesh(context, ob)
    bm.edges.ensure_lookup_table()
    for edge in bm.edges:
        pt0x, pt0y, pt0z = edge.verts[0].co
        pt1x, pt1y, pt1z = edge.verts[1].co
        xbs.append((pt0x, pt1x, pt0y, pt1y, pt0z, pt1z))
    bm.free()
    xbs.sort()
    if not xbs:
        raise BFException(ob, "XB: No exported edges!")
    msg = f"XB: {len(xbs)} edges"
    return xbs, msg


_choice_to_xbs = {
    "BBOX": _ob_to_xbs_bbox,
    "VOXELS": _ob_to_xbs_voxels,
    "FACES": _ob_to_xbs_faces,
    "PIXELS": _ob_to_xbs_pixels,
    "EDGES": _ob_to_xbs_edges,
}


def ob_to_xbs(context, ob) -> "((x0,x1,y0,y1,z0,z1,), ...), 'Msg'":
    """Transform Object geometry according to ob.bf_xb to FDS notation."""
    if not ob.get("ob_to_xbs_cache"):  # check cache
        print("BFDS: ob_to_xbs")
        ob["ob_to_xbs_cache"] = _choice_to_xbs[ob.bf_xb](context, ob)  # recalc
    return ob["ob_to_xbs_cache"]  # send cached


# to XYZ in Blender units


def _ob_to_xyzs_vertices(context, ob) -> "((x0,y0,z0,), ...), 'Msg'":
    """Transform Object vertices to xyzs notation."""
    xyzs = list()
    bm = utils.get_global_bmesh(context, ob)
    # For each vertex...
    bm.verts.ensure_lookup_table()
    for v in bm.verts:
        pt0x, pt0y, pt0z = v.co
        xyzs.append((pt0x, pt0y, pt0z))
    bm.free()
    xyzs.sort()
    if not xyzs:
        raise BFException(ob, "XYZ: No exported vertices!")
    msg = f"XYZ: {len(xyzs)} vertices"
    return xyzs, msg


def _ob_to_xyzs_center(context, ob) -> "((x0,y0,z0,), ...), 'Msg'":
    """Transform Object center to xyzs notation."""
    xyzs = [(ob.location[0], ob.location[1], ob.location[2])]
    msg = str()
    return xyzs, msg


_choice_to_xyzs = {"CENTER": _ob_to_xyzs_center, "VERTICES": _ob_to_xyzs_vertices}


def ob_to_xyzs(context, ob) -> "((x0,y0,z0,), ...), 'Msg'":
    """Transform Object geometry according to ob.bf_xyz to xyzs notation."""
    if not ob.get("ob_to_xyzs_cache"):  # check cache
        print("BFDS: ob_to_xyzs recalc:", ob.name)
        ob["ob_to_xyzs_cache"] = _choice_to_xyzs[ob.bf_xyz](context, ob)  # recalc
    return ob["ob_to_xyzs_cache"]  # send cached


# to PB in Blender units


def _ob_to_pbs_planes(context, ob) -> "((0,x3,), (1,x7,), (1,y9,), ...), 'Msg'":
    """Transform Object faces to pbs notation."""
    pbs = list()
    xbs, msg = _ob_to_xbs_faces(context, ob)
    epsilon = 1e-5
    # For each face build a plane...
    for xb in xbs:
        if abs(xb[1] - xb[0]) < epsilon:
            pbs.append((0, xb[0]))  # PBX is 0
        elif abs(xb[3] - xb[2]) < epsilon:
            pbs.append((1, xb[2]))  # PBY is 1
        elif abs(xb[5] - xb[4]) < epsilon:
            pbs.append((2, xb[4]))  # PBZ is 2
        else:
            raise ValueError(
                "BFDS: Building planes impossible, problem in ob_to_xbs_faces."
            )
    pbs.sort()
    if not pbs:
        raise BFException(ob, "PB*: No exported planes!")
    msg = f"PB*: {len(pbs)} planes"
    return pbs, msg


def ob_to_pbs(context, ob) -> "((0,x3,), (1,x7,), (1,y9,), ...), 'Msg'":
    """Transform Object geometry according to ob.bf_pb to pbs notation."""
    if not ob.get("ob_to_pbs_cache"):  # check cache
        print("BFDS: ob_to_pbs recalc:", ob.name)
        ob["ob_to_pbs_cache"] = _ob_to_pbs_planes(context, ob)  # recalc
    return ob["ob_to_pbs_cache"]  # send cached
