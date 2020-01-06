"""!
BlenderFDS, translate geometry from FDS notation to a Blender mesh.
"""

import bpy, logging
from time import time

log = logging.getLogger(__name__)

epsilon = 1e-5  # TODO unify epsilon mgmt

# From GEOM


def geom_to_mesh(fds_surfids, fds_verts, fds_faces, context, me, scale_length):
    """!
    Import GEOM vertices ((x0,y0,z0,), ...) and faces ((1,2,3,), ...) into existing Blender Mesh.
    @param fds_surfids: the fds surf id.
    @param fds_verts: the fds vertices.
    @param fds_faces: the fds faces.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    # Append material slots
    for i, surfid in enumerate(fds_surfids):
        found = False
        for ma in bpy.data.materials:
            if surfid == ma.name:
                me.materials.append(ma)
                found = True
                break
        if not found:
            raise Exception(f"Unknown SURF_ID <{surfid}>")
    # Treat fds_verts and fds_faces
    nverts, nfaces = len(fds_verts) // 3, len(fds_faces) // 4
    if nverts * 3 != len(fds_verts):
        raise Exception(f"Wrong VERTS len in <{fds_verts}>")
    if nfaces * 4 != len(fds_faces):
        raise Exception(f"Wrong FACES len in <{fds_faces}>")
    verts = [
        (
            fds_verts[i * 3] / scale_length,
            fds_verts[i * 3 + 1] / scale_length,
            fds_verts[i * 3 + 2] / scale_length,
        )
        for i in range(nverts)
    ]
    edges = list()
    faces = [
        (fds_faces[i * 4] - 1, fds_faces[i * 4 + 1] - 1, fds_faces[i * 4 + 2] - 1)
        for i in range(nfaces)
    ]
    imats = [fds_faces[i * 4 + 3] - 1 for i in range(nfaces)]
    # Check imats
    if max(imats) > len(me.materials) - 1:
        raise Exception(f"Wrong SURF_ID len in <{me.materials}>")
    # Create mesh
    me.from_pydata(verts, edges, faces)
    # Assign materials to faces
    for iface, face in enumerate(me.polygons):
        face.material_index = imats[iface]


def geom_to_ob(fds_surfids, fds_verts, fds_faces, context, ob, scale_length):
    """!
    Import GEOM vertices ((x0,y0,z0,), ...) and faces ((1,2,3,), ...) into existing Blender Mesh.
    @param fds_surfids: the fds surf id.
    @param fds_verts: the fds vertices.
    @param fds_faces: the fds faces.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param ob: the Blender Object.
    @param scale_length: the scale to use.
    """
    log.debug(ob.name)
    geom_to_mesh(fds_surfids, fds_verts, fds_faces, context, ob.data, scale_length)


# from XB in Blender units


def xbs_edges_to_mesh(xbs, context, me, scale_length):
    """!
    Import xbs edges ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        j = i * 2
        verts.extend(((x0, y0, z0), (x1, y1, z1)))
        edges.append((0 + j, 1 + j))
    me.from_pydata(verts, edges, faces)


def xbs_faces_to_mesh(xbs, context, me, scale_length):
    """!
    Import xbs faces ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        j = i * 4
        if abs(x1 - x0) <= epsilon:
            verts.extend(((x0, y0, z0), (x0, y1, z0), (x0, y1, z1), (x0, y0, z1)))
        elif abs(y1 - y0) <= epsilon:
            verts.extend(((x0, y0, z0), (x1, y0, z0), (x1, y0, z1), (x0, y0, z1)))
        elif abs(z1 - z0) <= epsilon:
            verts.extend(((x0, y0, z0), (x0, y1, z0), (x1, y1, z0), (x1, y0, z0)))
        else:
            raise Exception(f"Unrecognized XB as face <{xb}>")
        faces.append((0 + j, 1 + j, 2 + j, 3 + j))
    me.from_pydata(verts, edges, faces)


def xbs_bbox_to_mesh(xbs, context, me, scale_length):
    """!
    Import xbs bboxes ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        j = i * 8
        verts.extend(
            (
                (x0, y0, z0),
                (x1, y0, z0),
                (x1, y1, z0),
                (x0, y1, z0),
                (x0, y0, z1),
                (x1, y0, z1),
                (x1, y1, z1),
                (x0, y1, z1),
            )
        )
        faces.extend(
            (
                (0 + j, 3 + j, 2 + j, 1 + j),
                (0 + j, 1 + j, 5 + j, 4 + j),
                (0 + j, 4 + j, 7 + j, 3 + j),
                (6 + j, 5 + j, 1 + j, 2 + j),
                (6 + j, 2 + j, 3 + j, 7 + j),
                (6 + j, 7 + j, 4 + j, 5 + j),
            )
        )
    me.from_pydata(verts, edges, faces)


xbs_to_mesh = {
    "BBOX": xbs_bbox_to_mesh,
    "VOXELS": xbs_bbox_to_mesh,
    "PIXELS": xbs_bbox_to_mesh,
    "EDGES": xbs_edges_to_mesh,
    "FACES": xbs_faces_to_mesh,
}


def xbs_to_ob(xbs, context, ob, scale_length, ma=None, bf_xb=None) -> "bf_xb":
    """!
    Import xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Object.
    @param xbs: the xbs edges.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES
    @return ???
    """
    log.debug(ob.name)
    if bf_xb:  # force bf_xb
        xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
    else:  # auto choose
        try:
            bf_xb = "FACES"
            xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
        except:
            bf_xb = "BBOX"
            xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
    if ma:
        ob.active_material = ma
    return bf_xb


# From XYZ in Blender units


def xyzs_to_mesh(xyzs, context, me, scale_length):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param xyzs: the xyzs vertices.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    xyzs = tuple(
        (
            (xyz[0] / scale_length, xyz[1] / scale_length, xyz[2] / scale_length)
            for xyz in xyzs
        )
    )
    me.from_pydata(xyzs, tuple(), tuple())  # verts, edges, faces


def xyzs_to_ob(xyzs, context, ob, scale_length, ma=None) -> "bf_xyz":
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @return ???
    """
    xyzs_to_mesh(xyzs, context, ob.data, scale_length)
    if ma:
        ob.active_material = ma
    return "VERTICES"


# From PB


def pbs_to_mesh(pbs, context, me, scale_length):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param pbs: the pbs planes.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    xbs = list()
    for pb in pbs:
        sl = scale_length
        if pb[0] == 0:
            xbs.append((pb[1], pb[1], -sl, +sl, -sl, +sl))  # PBX is 0
        elif pb[0] == 1:
            xbs.append((-sl, +sl, pb[1], pb[1], -sl, +sl))  # PBY is 1
        elif pb[0] == 2:
            xbs.append((-sl, +sl, -sl, +sl, pb[1], pb[1]))  # PBZ is 2
        else:
            raise Exception(f"Unrecognized PB* <{pb}>")
    xbs_faces_to_mesh(xbs, context, me, scale_length)


def pbs_to_ob(pbs, context, ob, scale_length, ma=None) -> "bf_pb":
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param pbs: the pbs planes.
    @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @return ???
    """
    pbs_to_mesh(pbs, context, ob.data, scale_length)
    if ma:
        ob.active_material = ma
    return "PLANES"
