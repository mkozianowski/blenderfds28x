"""BlenderFDS, algorithms for triangulated surfaces."""

import bpy, bmesh, mathutils
from time import time
from math import floor, ceil

from ..lib.exceptions import BFException
from . import utils


# Get triangulated surface


def get_trisurface(context, ob, check=True) -> "mas, verts, faces":
    assert ob
    # Check object type
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise Exception("Object can not be converted to mesh")
    # Check original mesh quality
    if check:
        check_mesh_quality(context, ob)
    # Get ob materials from slots
    mas = list()
    material_slots = ob.material_slots
    if len(material_slots) == 0:
        raise BFException(ob, "No referenced SURF, add at least one Material")
    for material_slot in material_slots:
        ma = material_slot.material
        if not ma:
            raise BFException(
                ob, "No referenced SURF, fill empty slot with at least one Material"
            )
        if not ma.bf_export:
            raise BFException(ob, f"Referenced SURF <{ma.name}> is not exported")
        mas.append(ma.name)
    # Add triangulate modifier to original object
    mo = ob.modifiers.new("triangulate_tmp", "TRIANGULATE")
    mo.quad_method, mo.ngon_method = "BEAUTY", "BEAUTY"
    # Get evaluated ob (eg. modifiers applied)
    dg = bpy.context.evaluated_depsgraph_get()
    ob_eval = ob.evaluated_get(dg)
    me_eval = ob_eval.to_mesh()
    # Rm triangulate modifier from original ob
    ob.modifiers.remove(mo)
    # Get ob verts and faces # FIXME bmesh not needed
    bm = bmesh.new()
    bm.from_mesh(me_eval)
    verts = [(v.co.x, v.co.y, v.co.z) for v in bm.verts]
    faces = [
        (
            f.verts[0].index + 1,  # FDS index start from 1, not 0
            f.verts[1].index + 1,
            f.verts[2].index + 1,
            f.material_index + 1,
        )
        for f in bm.faces
    ]
    # Clean up
    bm.free()
    ob_eval.to_mesh_clear()
    return mas, verts, faces


# Check mesh quality FIXME modifies original ob!


def check_mesh_quality(context, ob):
    """Check that Object is a closed orientable manifold,
    with no degenerate geometry."""
    # Init
    print("BFDS: Check mesh quality")
    bpy.ops.object.mode_set(mode="OBJECT")
    bm = bmesh.new()
    bm.from_mesh(ob.data)
    bm.faces.ensure_lookup_table()  # update bmesh index
    bm.verts.ensure_lookup_table()  # update bmesh index
    bm.edges.ensure_lookup_table()  # update bmesh index
    epsilon_len = context.scene.bf_config_min_edge_length
    epsilon_area = context.scene.bf_config_min_face_area
    bad_verts, bad_edges, bad_faces = list(), list(), list()
    # Check manifold vertices
    for vert in bm.verts:
        if not vert.is_manifold:
            bad_verts.append(vert)
    if bad_verts:
        msg = "Non manifold vertices detected, bad vertices selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_verts=bad_verts)
    # Check manifold edges, each edge should join two faces, no more no less
    for edge in bm.edges:
        if not edge.is_manifold:
            bad_edges.append(edge)
    if bad_edges:
        msg = "Non manifold or open geometry detected, bad edges selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_edges=bad_edges)
    # Check contiguous normals, adjoining faces should have normals
    # in the same directions
    for edge in bm.edges:
        if not edge.is_contiguous:
            bad_edges.append(edge)
    if bad_edges:
        msg = "Inconsistent face normals detected, bad edges selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_edges=bad_edges)
    # Check no degenerate edges, zero lenght edges
    for edge in bm.edges:
        if edge.calc_length() <= epsilon_len:
            bad_edges.append(edge)
    if bad_edges:
        msg = "Too short edges detected, bad edges selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_edges=bad_edges)
    # Check degenerate faces, zero area faces
    for face in bm.faces:
        if face.calc_area() <= epsilon_area:
            bad_faces.append(face)
    if bad_faces:
        msg = "Too small area faces detected, bad faces selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_faces=bad_faces)
    # Check loose vertices, vertices that have no connectivity
    for vert in bm.verts:
        if not bool(vert.link_edges):
            bad_verts.append(vert)
    if bad_verts:
        msg = "Loose vertices detected, bad vertices selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_verts=bad_verts)
    # Check duplicate vertices
    _check_duplicate_vertices(context, ob, bm, epsilon_len)
    # Set overall normals
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    bm.to_mesh(ob.data)
    # Close
    bm.free()


def _check_duplicate_vertices(context, ob, bm, epsilon_len):
    """Check duplicate vertices."""
    bad_verts = list()
    size = len(bm.verts)
    kd = mathutils.kdtree.KDTree(size)  # create a kd-tree from a mesh
    for i, vert in enumerate(bm.verts):
        kd.insert(vert.co, i)
    kd.balance()
    for vert in bm.verts:
        vert_group = list()
        for (_, i, _) in kd.find_range(vert.co, epsilon_len):
            vert_group.append(i)
        if len(vert_group) > 1:
            for i in vert_group:
                bad_verts.append(bm.verts[i])
    if bad_verts:
        msg = "Duplicate vertices detected, bad vertices selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_verts=bad_verts)


# Check intersections


def _get_bm_and_tree(context, ob, epsilon_len=0.0, matrix=None):
    """Get BMesh and BVHTree from Object."""
    bm = bmesh.new()  # remains in ob local coordinates
    depsgraph = context.evaluated_depsgraph_get()
    bm.from_object(ob, depsgraph=depsgraph, deform=True, cage=False, face_normals=True)
    bm.faces.ensure_lookup_table()  # update bmesh index
    if matrix:
        bm.transform(matrix)
    tree = mathutils.bvhtree.BVHTree.FromBMesh(bm, epsilon=epsilon_len)
    return bm, tree


def _get_intersected_faces(bm, tree, other_tree):
    """Get intersected faces between trees."""
    overlap = tree.overlap(other_tree)
    if overlap:
        ifaces = {i_pair[0] for i_pair in overlap}
        return [bm.faces[iface] for iface in ifaces]
    return list()


def check_intersections(context, ob, other_obs=None):
    """Check ob self-intersection and intersection with other_obs."""
    print(
        f"BFDS: Check self intersections in Object <{ob.name}>, and intersections with other selected obs"
    )
    bpy.ops.object.mode_set(mode="OBJECT")
    epsilon_len = context.scene.bf_config_min_edge_length
    bad_faces = list()
    bm, tree = _get_bm_and_tree(context, ob, epsilon_len=epsilon_len)
    # Get self-intersections
    bad_faces.extend(_get_intersected_faces(bm, tree, tree))
    # Get intersections
    for other_ob in other_obs or tuple():
        matrix = (
            ob.matrix_world.inverted() @ other_ob.matrix_world
        )  # Blender 2.80 matrix multiplication
        _, other_tree = _get_bm_and_tree(
            context, other_ob, epsilon_len=epsilon_len, matrix=matrix
        )
        bad_faces.extend(_get_intersected_faces(bm, tree, other_tree))
    # Raise
    if bad_faces:
        msg = "Intersection detected, bad faces selected."
        _raise_bad_geometry(context, ob, bm, msg, bad_faces=bad_faces)


# Raise bad geometry


def _raise_bad_geometry(
    context, ob, bm, msg, bad_verts=None, bad_edges=None, bad_faces=None
):
    """Select bad elements, show them, raise BFException."""
    # Deselect all in bmesh
    for vert in bm.verts:
        vert.select = False
    bm.select_flush(False)
    # Select bad elements
    select_type = None
    if bad_faces:
        select_type = "FACE"
        for b in bad_faces:
            b.select = True
    if bad_edges:
        select_type = "EDGE"
        for b in bad_edges:
            b.select = True
    if bad_verts:
        select_type = "VERT"
        for b in bad_verts:
            b.select = True
    bm.to_mesh(ob.data)
    bm.free()
    # Select object and go to edit mode
    bpy.ops.object.mode_set(mode="OBJECT")
    bpy.ops.object.select_all(action="DESELECT")
    ob.select_set(True)  # Blender 2.80
    context.view_layer.objects.active = ob  # Blender 2.80
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_mode(use_extend=False, use_expand=False, type=select_type)
    raise BFException(ob, msg)
