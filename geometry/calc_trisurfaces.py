"""BlenderFDS, algorithms for triangulated surfaces."""

import bpy, bmesh, mathutils
from time import time
from math import floor, ceil

from ..lib.exceptions import BFException
from . import utils


# Get triangulated surface

# FIXME updates object after calculation
# FIXME check mesh for quality


def get_trisurface(context, ob, scale_length, check=True) -> "mas, verts, faces":
    """Get triangulated surface from object in xbs format."""
    print("BFDS: calc_voxels.get_trisurface:", ob.name)
    mas = _get_materials(context, ob)
    bm = _get_prepared_bmesh(context, ob)
    # Check
    if check:
        _check_bm_quality(context, ob, bm)
    # Extract verts and faces from bmesh
    verts = [
        (v.co.x * scale_length, v.co.y * scale_length, v.co.z * scale_length)
        for v in bm.verts
    ]
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
    return mas, verts, faces


def _get_prepared_bmesh(context, ob):
    """Prepare ob into a triangulated bmesh in world coordinates."""
    # Check object and init
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise BFException(ob, "Object can not be converted to mesh")
    if not ob.data.vertices:
        raise BFException(ob, "Empty object!")
    # Get evaluated bmesh from ob
    bm = utils.get_object_bmesh(context, ob, world=True)
    bmesh.ops.triangulate(bm, faces=bm.faces)
    # Update bmesh index for reference
    bm.faces.ensure_lookup_table()
    bm.verts.ensure_lookup_table()
    bm.edges.ensure_lookup_table()
    return bm


def _get_materials(context, ob):
    """Get ob materials from slots."""
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
    return mas


# FIXME modifies original ob! Maybe triangulate modif?


def check_mesh_quality(context, ob):
    """Check that Object is a closed orientable manifold,
    with no degenerate geometry."""
    bm = _get_prepared_bmesh(context, ob)
    _check_bm_quality(context, ob, bm)
    # Clean up
    bm.free()


def _check_bm_quality(context, ob, bm):
    """Check that bmesh is a closed orientable manifold, with no degenerate geometry."""
    epsilon_len = context.scene.bf_config_min_edge_length
    epsilon_area = context.scene.bf_config_min_face_area
    _check_manifold_vertices(context, ob, bm, epsilon_len, epsilon_area)
    _check_manifold_edges(context, ob, bm, epsilon_len, epsilon_area)
    _check_contiguous_normals(context, ob, bm, epsilon_len, epsilon_area)
    _check_degenerate_edges(context, ob, bm, epsilon_len, epsilon_area)
    _check_degenerate_faces(context, ob, bm, epsilon_len, epsilon_area)
    _check_loose_vertices(context, ob, bm, epsilon_len, epsilon_area)
    _check_normal_orientation(context, ob, bm, epsilon_len, epsilon_area)
    _check_duplicate_vertices(context, ob, bm, epsilon_len, epsilon_area)


def _check_manifold_vertices(context, ob, bm, epsilon_len, epsilon_area):
    """Check manifold vertices."""
    bad_verts = list()
    for vert in bm.verts:
        if not vert.is_manifold:
            bad_verts.append(vert)
    if bad_verts:
        msg = f"Non manifold vertices detected ({len(bad_verts)} vertices)."
        raise BFException(ob, msg)


def _check_manifold_edges(context, ob, bm, epsilon_len, epsilon_area):
    """Check manifold edges, each edge should join two faces, no more no less."""
    bad_edges = list()
    for edge in bm.edges:
        if not edge.is_manifold:
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Non manifold or open geometry detected ({len(bad_edges)} edges)."
        raise BFException(ob, msg)


def _check_contiguous_normals(context, ob, bm, epsilon_len, epsilon_area):
    """Check contiguous normals, adjoining faces should have normals in the same directions"""
    bad_edges = list()
    for edge in bm.edges:
        if not edge.is_contiguous:
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Inconsistent face normals detected ({len(bad_edges)} edges)."
        raise BFException(ob, msg)


def _check_degenerate_edges(context, ob, bm, epsilon_len, epsilon_area):
    """Check no degenerate edges, zero lenght edges."""
    bad_edges = list()
    for edge in bm.edges:
        if edge.calc_length() <= epsilon_len:
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Too short edges detected ({len(bad_edges)} edges)."
        raise BFException(ob, msg)


def _check_degenerate_faces(context, ob, bm, epsilon_len, epsilon_area):
    """Check degenerate faces, zero area faces."""
    bad_faces = list()
    for face in bm.faces:
        if face.calc_area() <= epsilon_area:
            bad_faces.append(face)
    if bad_faces:
        msg = f"Too small area faces detected ({len(bad_faces)} faces)."
        raise BFException(ob, msg)


def _check_loose_vertices(context, ob, bm, epsilon_len, epsilon_area):
    """Check loose vertices, vertices that have no connectivity."""
    bad_verts = list()
    for vert in bm.verts:
        if not bool(vert.link_edges):
            bad_verts.append(vert)
    if bad_verts:
        msg = f"Loose vertices detected ({len(bad_verts)} vertices)."
        raise BFException(ob, msg)


def _check_normal_orientation(context, ob, bm, epsilon_len, epsilon_area):
    """CHeck overall normals."""
    # bad_verts, bad_edges, bad_faces = list(), list(), list()
    # bmesh.ops.recalc_face_normals(bm, faces=bm.faces)  # FIXME detect wrong side normals
    pass


def _check_duplicate_vertices(context, ob, bm, epsilon_len, epsilon_area):
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
        msg = f"Duplicate vertices detected ({len(bad_verts)} vertices)."
        raise BFException(ob, msg)


# Check intersections # FIXME


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


# FIXME
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
        msg = "Intersection detected."
        raise BFException(ob, msg)


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
