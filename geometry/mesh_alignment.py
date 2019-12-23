"""BlenderFDS, mesh alignment experiments."""

# Before:
#   |   |rcs|   |   | ri  ref mesh
#   ·---·---·---·---·
#  rx0   <-rl->    rx1
# mx0      <-ml->      mx1
#  ·------·------·------·
#  |      | mcs  |      | mi  other mesh
# ---> axis

# After:
#  |   |   |   |   |
#  ·---·---·---·---·-------·
#  |       |       |       |

# Not allowed:
#  |    |     |    |
#  ·----·--·--·----·
#  |       |       |

#  |   |   |   |
#  ·---·---·---·---·
#  |       |       |


def _factor(n):
    """Generator for prime factors of n.
Many thanks Dhananjay Nene (http://dhananjaynene.com/)
for publishing this code"""
    yield 1
    i = 2
    limit = n ** 0.5
    while i <= limit:
        if n % i == 0:
            yield i
            n = n / i
            limit = n ** 0.5
        else:
            i += 1
    if n > 1:
        yield int(n)


def _n_for_poisson(n):
    """Get a good number for poisson solver at least bigger than n."""
    good = set((1, 2, 3, 5))
    while True:
        if [i for i in _factor(n) if i not in good]:
            n += 1
        else:
            break
    return n


def _align_along_axis(ri, rx0, rx1, mi, mx0, mx1, poisson=False, protect_rl=False):
    """Align coarse MESH to fixed ref MESH along an axis."""
    # Init
    assert rx0 < rx1 and mx0 < mx1  # coordinate order
    rl, ml = rx1 - rx0, mx1 - mx0  # lengths
    rcs, mcs = rl / ri, ml / mi  # cell sizes
    # Coarsening ratio
    assert mcs / rcs > 0.501  # same or coarser allowed, protect from float err
    n = round(mcs / rcs)
    # Set ref cell count multiple of n
    # to allow full cover of coarse cells by ref cells
    ri = round(ri / n) * n
    if poisson:
        ri = _n_for_poisson(ri)
    if protect_rl:  # protect ref length
        rcs = rl / ri  # reduce ref cell size
    else:
        rl = rcs * ri  # extend ref length due to updated ri
        rx1 = rx0 + rl
    # Calc new coarse cell size from ref cell size
    mcs = rcs * n
    # Calc new coarse cell count,
    # trying to keep ml as close as possible to original
    mi = round(ml / mcs)
    if poisson:
        mi = _n_for_poisson(mi)
    # Align coarse mesh positions to ref mesh
    mx0 = rx0 + round((mx0 - rx0) / mcs) * mcs
    ml = mcs * mi  # extend other mesh due to updated mi
    mx1 = mx0 + ml
    print("n:", n, "rcs:", rcs, "mcs:", mcs)
    return ri, rx0, rx1, mi, mx0, mx1


def _align_along_x(rijks, rxbs, mijks, mxbs, poisson=False, protect_rl=False):
    """Align coarse MESH to fixed ref MESH along axis x."""
    rijks[0], rxbs[0], rxbs[1], mijks[0], mxbs[0], mxbs[1] = _align_along_axis(
        ri=rijks[0],
        rx0=rxbs[0],
        rx1=rxbs[1],
        mi=mijks[0],
        mx0=mxbs[0],
        mx1=mxbs[1],
        poisson=False,  # not needed along x
        protect_rl=protect_rl,
    )


def _align_along_y(rijks, rxbs, mijks, mxbs, poisson=False, protect_rl=False):
    """Align coarse MESH to fixed ref MESH along axis y."""
    rijks[1], rxbs[2], rxbs[3], mijks[1], mxbs[2], mxbs[3] = _align_along_axis(
        ri=rijks[1],
        rx0=rxbs[2],
        rx1=rxbs[3],
        mi=mijks[1],
        mx0=mxbs[2],
        mx1=mxbs[3],
        poisson=poisson,
        protect_rl=protect_rl,
    )


def _align_along_z(rijks, rxbs, mijks, mxbs, poisson=False, protect_rl=False):
    """Align coarse MESH to fixed ref MESH along axis z."""
    rijks[2], rxbs[4], rxbs[5], mijks[2], mxbs[4], mxbs[5] = _align_along_axis(
        ri=rijks[2],
        rx0=rxbs[4],
        rx1=rxbs[5],
        mi=mijks[2],
        mx0=mxbs[4],
        mx1=mxbs[5],
        poisson=poisson,
        protect_rl=protect_rl,
    )


# rx0 rx1
#  .---.
#       delta .----.
#            mx0  mx1


def _is_far(rxbs, mxbs, deltas):
    return (
        rxbs[0] - mxbs[1] > deltas[0]
        or mxbs[0] - rxbs[1] > deltas[0]
        or rxbs[2] - mxbs[3] > deltas[1]  # x
        or mxbs[2] - rxbs[3] > deltas[1]
        or rxbs[4] - mxbs[5] > deltas[2]  # y
        or mxbs[4] - rxbs[5] > deltas[2]  # z
    )


def align_meshes(rijks, rxbs, mijks, mxbs, poisson=False, protect_rl=False):
    # Init
    deltas = (  # rcs
        abs(rxbs[0] - rxbs[1]) / rijks[0],
        abs(rxbs[2] - rxbs[3]) / rijks[1],
        abs(rxbs[4] - rxbs[5]) / rijks[2],
    )
    # Are meshes far apart?
    if _is_far(rxbs=rxbs, mxbs=mxbs, deltas=deltas):
        print("Meshes are far apart, no modification")
        return rijks, rxbs, mijks, mxbs
    # If mesh sides are close, then snap them
    # otherwise align their meshes
    if abs(rxbs[0] - mxbs[1]) <= deltas[0]:  # -x close?
        print("Snapping -x")
        mxbs[1] = rxbs[0]
    elif abs(mxbs[0] - rxbs[1]) <= deltas[0]:  # +x close?
        print("Snapping +x")
        mxbs[0] = rxbs[1]
    else:
        print("Aligning x")
        _align_along_x(rijks, rxbs, mijks, mxbs, poisson, protect_rl)
    if abs(rxbs[2] - mxbs[3]) <= deltas[1]:  # -y close?
        print("Snapping -y")
        mxbs[3] = rxbs[2]
    elif abs(mxbs[2] - rxbs[3]) <= deltas[1]:  # +y close?
        print("Snapping +y")
        mxbs[2] = rxbs[3]
    else:
        print("Aligning y")
        _align_along_y(rijks, rxbs, mijks, mxbs, poisson, protect_rl)
    if abs(rxbs[4] - mxbs[5]) <= deltas[2]:  # -z close?
        print("Snapping -z")
        mxbs[5] = rxbs[4]
    elif abs(mxbs[4] - rxbs[5]) <= deltas[2]:  # +z close?
        print("Snapping +z")
        mxbs[4] = rxbs[5]
    else:
        print("Aligning z")
        _align_along_z(rijks, rxbs, mijks, mxbs, poisson, protect_rl)
    print("ref:", rijks, rxbs)
    print("oth:", mijks, mxbs)
    return rijks, rxbs, mijks, mxbs


def test():
    print("snap -z")
    align_meshes(  # -z
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, 0.0, 5.0, 0.0, 5.0],
        mijks=[70, 34, 20],
        mxbs=[5.01, 10.0, 0.0, 5.0, -5.0, 0.05],
        poisson=True,
        protect_rl=True,
    )
    print("snap +z")
    align_meshes(  # +z
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, 0.0, 5.0, -5.0, 0.0],
        mijks=[70, 34, 20],
        mxbs=[5.1, 10.0, 0.0, 5.0, 0.01, 4.9],
        poisson=True,
        protect_rl=True,
    )
    print("align -z")
    align_meshes(  # -z
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, 0.0, 5.0, 0.0, 5.0],
        mijks=[70, 34, 20],
        mxbs=[0.1, 5.0, 0.0, 5.0, -5.0, 0.05],
        poisson=True,
        protect_rl=True,
    )
    print("align +z")
    align_meshes(  # +z
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, 0.0, 5.0, -5.0, 0.0],
        mijks=[70, 34, 20],
        mxbs=[0.1, 5.0, 0.0, 5.0, 0.01, 4.9],
        poisson=True,
        protect_rl=True,
    )
    print("align -x")
    align_meshes(  # -x
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, -1, 4.1, 2, 6.0],
        mijks=[70, 38, 20],
        mxbs=[-3, 0.01, 0.0, 5.0, 0, 5.0],
        poisson=True,
        protect_rl=True,
    )
    print("align +x")
    align_meshes(  # +x
        rijks=[151, 37, 51],
        rxbs=[0.0, 5.0, -1, 4.1, 2, 6.0],
        mijks=[70, 38, 20],
        mxbs=[5.01, 7.01, 0.0, 5.0, 0, 5.0],
        poisson=True,
        protect_rl=True,
    )
    print("align -y")
    align_meshes(  # -y
        rijks=[151, 37, 51],
        rxbs=[-5, -1, -3, -0.01, 2, 6.0],
        mijks=[70, 38, 20],
        mxbs=[-3, 0.01, 0.0, 5.0, 0, 5.0],
        poisson=True,
        protect_rl=True,
    )
    print("align +y")
    align_meshes(  # +y
        rijks=[151, 37, 51],
        rxbs=[6, 9, 5.01, 8.1, 2, 6.0],
        mijks=[70, 38, 20],
        mxbs=[5.01, 7.01, 0.0, 5.0, 0, 5.0],
        poisson=True,
        protect_rl=True,
    )
    print()
    print()
    print("Test")
    align_meshes(
        rijks=[15, 37, 51],
        rxbs=[0, 5, 0, 5, 0, 5],
        mijks=[9, 38, 20],
        mxbs=[0, 5, -5, 0, -5, 0],
        poisson=True,
        protect_rl=True,
    )


test()
