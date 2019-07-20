"""BlenderFDS, translate geometry from FDS notation to a Blender mesh."""

import bpy
from time import time

from . import utils

#++ from None

def none_to_mesh(value=None, me=None) -> "Mesh":
    """Transform nothing to Blender mesh."""
    return me or bpy.data.meshes.new("none")

#++ from XB

def xbs_edges_to_mesh(xbs, me=None) -> "Mesh":
    """Translate XB edges ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    if not me:
        me = bpy.data.meshes.new("xbs_edges")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 2
        verts.extend(((x0,y0,z0), (x1,y1,z1)))
        edges.append((0+j,1+j))
    me.from_pydata(verts, edges, faces)
    return me

def xbs_faces_to_mesh(xbs, me=None) -> "Mesh":
    """Translate XB faces ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    epsilon = 1E-5
    if not me:
        me = bpy.data.meshes.new("xbs_faces")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 4
        if   abs(x1 - x0) < epsilon:
            verts.extend(((x0,y0,z0), (x0,y1,z0), (x0,y1,z1), (x0,y0,z1)))
        elif abs(y1 - y0) < epsilon:
            verts.extend(((x0,y0,z0), (x1,y0,z0), (x1,y0,z1), (x0,y0,z1)))
        elif abs(z1 - z0) < epsilon:
            verts.extend(((x0,y0,z0), (x0,y1,z0), (x1,y1,z0), (x1,y0,z0)))
        else:
            print("BFDS: from_fds.xbs_faces_to_ob: this XB is not a face:", xb)
            continue
        faces.append((0+j,1+j,2+j,3+j))
    me.from_pydata(verts, edges, faces)
    return me

def xbs_bbox_to_mesh(xbs, me=None) -> "Mesh":
    """Translate XB bbox ((x0,x1,y0,y1,z0,z1,), ...) to Blender mesh."""
    if not me:
        me = bpy.data.meshes.new("xbs_bbox")
    verts, edges, faces = list(), list(), list()
    for i, xb in enumerate(xbs):
        x0, x1, y0, y1, z0, z1 = xb
        j = i * 8
        verts.extend(((x0,y0,z0), (x1,y0,z0), (x1,y1,z0), (x0,y1,z0), (x0,y0,z1), (x1,y0,z1), (x1,y1,z1), (x0,y1,z1)))
        faces.extend(((0+j,3+j,2+j,1+j), (0+j,1+j,5+j,4+j), (0+j,4+j,7+j,3+j), (6+j,5+j,1+j,2+j), (6+j,2+j,3+j,7+j), (6+j,7+j,4+j,5+j)))
    me.from_pydata(verts, edges, faces)
    return me

# Caller function
# If no ob, a new one (named name) is created and returned
# If no bf_xb, bf_xyz, bf_pb, a guess is made from data

choose_from_xbs = {
    "NONE"   : none_to_mesh,
    "BBOX"   : xbs_bbox_to_mesh,
    "VOXELS" : xbs_bbox_to_mesh,
    "FACES"  : xbs_faces_to_mesh,
    "PIXELS" : xbs_bbox_to_mesh,
    "EDGES"  : xbs_edges_to_mesh,
}

def xbs_to_ob(xbs, context, ob=None, bf_xb="NONE", name="xbs_to_ob", update_center=True) -> "Mesh":
    """Transform geometry in FDS notation to Blender object."""
    # Choose bf_xb
    epsilon = 1E-5
    if bf_xb == "NONE":
        x0, x1, y0, y1, z0, z1 = xbs[0]
        if abs(x1-x0) < epsilon or abs(y1-y0) < epsilon or abs(z1-z0) < epsilon:
            bf_xb = "FACES"
        else:
            bf_xb = "BBOX"
    # Get mesh, set it, set properties and center position
    me = choose_from_xbs[bf_xb](xbs)
    if ob:
        utils.set_global_mesh(context, ob, me) # ob exists, set its mesh
    else:
        ob = utils.get_new_object(context, context.scene, name, me) # no ob, get a new one with proper mesh
    if update_center:
        utils.set_balanced_center_position(context, ob)
    ob.bf_xb = bf_xb
    return ob

#++ from XYZ

def xyzs_vertices_to_mesh(xyzs, me=None) -> "Mesh":
    """Translate XYZ vertices ((x0,y0,z0,), ...) to Blender mesh."""
    if not me:
        me = bpy.data.meshes.new("xyzs_vertices")
    verts, edges, faces = xyzs, list(), list()
    me.from_pydata(verts, edges, faces)
    return me

# Caller function
# If no ob, a new one (named name) is created and returned
# If no bf_xb, bf_xyz, bf_pb, a guess is made from data

choose_from_xyzs = {
    "NONE"     : none_to_mesh,
    "CENTER"   : xyzs_vertices_to_mesh,
    "VERTICES" : xyzs_vertices_to_mesh,
}

def xyzs_to_ob(xyzs, context, ob=None, bf_xyz="NONE", name="xyzs_to_ob", update_center=True) -> "Mesh":
    """Transform geometry in FDS notation to Blender object."""
    # Choose bf_xyz
    if bf_xyz == "NONE":
        bf_xyz = "VERTICES"
    # Get mesh, set it, set properties and center position
    me = choose_from_xyzs[bf_xyz](xyzs)
    if ob:
        utils.set_global_mesh(context, ob, me) # ob exists, set its mesh
    else:
        ob = utils.get_new_object(context, context.scene, name, me) # no ob, get a new one with proper mesh
    if update_center:
        utils.set_balanced_center_position(context, ob)
    ob.bf_xyz = bf_xyz
    return ob

#++ from PB

def pbs_planes_to_mesh(pbs, me=None) -> "Mesh":
    """Translate PB* planes ((0,x3,), (0,x7,), (1,y9,), ...) to Blender mesh."""
    # Prepare xbs
    xbs = list()
    for i, pb in enumerate(pbs):
        if   pb[0] == 0: xbs.append((pb[1], pb[1], -1., +1., -1., +1.)) # PBX is 0
        elif pb[0] == 1: xbs.append((-1., +1., pb[1], pb[1], -1., +1.)) # PBY is 1
        elif pb[0] == 2: xbs.append((-1., +1., -1., +1., pb[1], pb[1])) # PBZ is 2
        else:
            print("BFDS: from_fds.pbs_planes_to_ob: unrecognized PB*:", pb)
            continue
    # Call companion function
    return xbs_faces_to_mesh(xbs, me)

# Caller function
# If no ob, a new one (named name) is created and returned
# If no bf_xb, bf_xyz, bf_pb, a guess is made from data

choose_from_pbs = {
    "NONE"   : none_to_mesh,
    "PLANES" : pbs_planes_to_mesh,
}

def pbs_to_ob(pbs, context, ob=None, bf_pb="NONE", name="pbs_to_ob", update_center=True) -> "Mesh":
    """Transform geometry in FDS notation to Blender object."""
    # Choose bf_pb
    if bf_pb == "NONE": bf_pb = "PLANES"
    # Get mesh, set it, set properties and center position
    me = choose_from_pbs[bf_pb](pbs)
    if ob:
        utils.set_global_mesh(context, ob, me) # ob exists, set its mesh
    else:
        ob = utils.get_new_object(context, context.scene, name, me) # no ob, get a new one with proper mesh
    if update_center:
        utils.set_balanced_center_position(context, ob)
    ob.bf_pb = bf_pb
    return ob

#++ from GEOM

def geom_to_mesh(fds_surfids, fds_verts, fds_faces, me=None) -> "Mesh":
    """Translate GEOM vertices ((x0,y0,z0,), ...) and faces ((1,2,3,), ...) to Blender mesh."""
    if not me:
        me = bpy.data.meshes.new("geom_to_mesh")
    # Append material slots
    for i, surfid in enumerate(fds_surfids):
        found = False
        for ma in bpy.data.materials:
            if surfid == ma.name:
                me.materials.append(ma)
                found = True
                break
        if not found:
            raise Exception("Unknown SURF_ID '{}'".format(surfid))
    # Treat fds_verts and fds_faces
    nverts, nfaces = len(fds_verts) // 3, len(fds_faces) // 4
    if nverts * 3 != len(fds_verts):
        raise Exception("Wrong VERTS length")
    if nfaces * 4 != len(fds_faces):
        raise Exception("Wrong FACES length")
    verts = [(fds_verts[i*3], fds_verts[i*3+1], fds_verts[i*3+2],) for i in range(nverts)]
    edges = list()
    faces = [(fds_faces[i*4]-1, fds_faces[i*4+1]-1, fds_faces[i*4+2]-1,) for i in range(nfaces)]
    imats = [fds_faces[i*4+3]-1 for i in range(nfaces)]
    # Check imats
    if max(imats) > len(me.materials)-1:
        raise Exception("Wrong SURF_ID length")
    # Create mesh
    me.from_pydata(verts, edges, faces)
    # Assign materials to faces
    for iface, face in enumerate(me.polygons):
        face.material_index = imats[iface]
    return me

def geom_to_ob(fds_surfids, fds_verts, fds_faces, context, ob=None, name="geom_to_ob", update_center=True) -> "Mesh":
    """Transform geometry in FDS notation to Blender object."""
    # Get mesh, set it, set properties and center position
    me = geom_to_mesh(fds_surfids, fds_verts, fds_faces, me=None)
    if ob:
        utils.set_global_mesh(context, ob, me) # ob exists, set its mesh
    else:
        ob = utils.get_new_object(context, context.scene, name, me) # no ob, get a new one with proper mesh
    if update_center:
        utils.set_balanced_center_position(context, ob)
    return ob
