"""BlenderFDS, mesh alignment experiments."""

# Before:
#  |  |  |  |  | n0, cs0
#  ·--·--·--·--·
#
# ·----·----·----·
# |    |    |    | n1, cs1
#
# After:
#  |  |  |  |  | n0, cs0
#  ·--·--·--·--·
#  |     |     | n1, cs1


def get_multiple_cell_size(ref_size, orig_size):
    """Get cell size that is exact multiple of ref_size,
    and as close as possible to orig_size. """
    if orig_size < ref_size:
        return ref_size / round(ref_size / orig_size)
    else:
        return ref_size * round(orig_size / ref_size)


def is_close(c0, c1, ref_size):
    """If orig_coo is close enough to ref_coo return True."""
    return abs(c1 - c0) <= ref_size


def is_overlapping(interval0, interval1):
    """Return True if intervals are overlapping."""
    return max(interval0) > min(interval1) and min(interval0) < max(interval1)


def get_cs(xb, ijk):
    """Return MESH cell size."""
    return (
        abs(xb[0] - xb[1]) / ijk[0],
        abs(xb[2] - xb[3]) / ijk[1],
        abs(xb[4] - xb[5]) / ijk[2],
    )


# ref mesh
xb0 = [0.0, 5.0, 0.0, 5.0, 0.0, 5.0]
ijk0 = [10, 10, 10]
cs0 = get_cs(xb0, ijk0)

# aligning mesh
xb1 = [5.5, 10.0, 2.0, 6.0, 2.0, 6.0]
ijk1 = [5, 5, 5]
cs1 = get_cs(xb1, ijk1)

if (
    is_close(xb0[1], xb1[0], cs0[0])
    and is_overlapping(xb0[2:4], xb1[2:4])
    and is_overlapping(xb0[4:], xb1[4:])
):
    # Close enough along +x and overlapping along y and z
    # FIXME check also along -x -y +y -z +z
    xb1[0] = xb0[1]  # touch on +x

    # Update second mesh size along y and z
    # - align correctly with first mesh in y and z
    # - set the right xb1 size along y and z, to obtain right cell_size with given j and k
    size_y = get_mesh_size(ref_size, orig_size)
    size_z = get_mesh_size(ref_size, orig_size)
    # Update j, xb1[2], xb1[3]

    # Update k, xb1[4], xb1[5]

