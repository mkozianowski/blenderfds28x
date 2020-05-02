"""!
BlenderFDS, mesh alignment.
"""

import csv
import math

# Mesh alignment:
#
# Before:
#   |   |rcs|   |   | ri  ref mesh
#   ·---·---·---·---·
#  rx0   <-rl->    rx1
# mx0      <-ml->      mx1
#  ·------·------·------·
#  |      | mcs  |      | mi  other mesh
# ---> axis

# Either protect rl or rcs.
# Instead ml and mcs are changed for alignment.

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
    """!
    Generator for prime factors of n.
    Many thanks Dhananjay Nene (http://dhananjaynene.com/)
    for publishing this code
    """
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
    """!Get a good number for poisson solver at least bigger than n."""
    good = set((1, 2, 3, 5))
    while True:
        if [i for i in _factor(n) if i not in good]:
            n += 1
        else:
            break
    return n


def _align_along_axis(ri, rx0, rx1, mi, mx0, mx1, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along an axis."""
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
    # trying to keep ml as close as possible to the original
    mi = round(ml / mcs)
    if poisson:
        mi = _n_for_poisson(mi)
    # Align coarse mesh positions to the ref mesh
    mx0 = rx0 + round((mx0 - rx0) / mcs) * mcs
    ml = mcs * mi  # extend other mesh due to updated mi
    mx1 = mx0 + ml
    return ri, rx0, rx1, mi, mx0, mx1


def _align_along_x(rijk, rxbs, mijk, mxbs, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis x."""
    rijk[0], rxbs[0], rxbs[1], mijk[0], mxbs[0], mxbs[1] = _align_along_axis(
        ri=rijk[0],
        rx0=rxbs[0],
        rx1=rxbs[1],
        mi=mijk[0],
        mx0=mxbs[0],
        mx1=mxbs[1],
        poisson=False,  # not needed along x
        protect_rl=protect_rl,
    )


def _align_along_y(rijk, rxbs, mijk, mxbs, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis y."""
    rijk[1], rxbs[2], rxbs[3], mijk[1], mxbs[2], mxbs[3] = _align_along_axis(
        ri=rijk[1],
        rx0=rxbs[2],
        rx1=rxbs[3],
        mi=mijk[1],
        mx0=mxbs[2],
        mx1=mxbs[3],
        poisson=poisson,  # needed along y
        protect_rl=protect_rl,
    )


def _align_along_z(rijk, rxbs, mijk, mxbs, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis z."""
    rijk[2], rxbs[4], rxbs[5], mijk[2], mxbs[4], mxbs[5] = _align_along_axis(
        ri=rijk[2],
        rx0=rxbs[4],
        rx1=rxbs[5],
        mi=mijk[2],
        mx0=mxbs[4],
        mx1=mxbs[5],
        poisson=poisson,  # needed along z
        protect_rl=protect_rl,
    )


# rx0 rx1
#  .---.
#       delta .----.
#            mx0  mx1


def _is_far(rxbs, mxbs, deltas):
    return (
        rxbs[0] - mxbs[1] > deltas[0]
        or mxbs[0] - rxbs[1] > deltas[0]  # x
        or rxbs[2] - mxbs[3] > deltas[1]
        or mxbs[2] - rxbs[3] > deltas[1]  # y
        or rxbs[4] - mxbs[5] > deltas[2]
        or mxbs[4] - rxbs[5] > deltas[2]  # z
    )


def align_meshes(rijk, rxbs, mijk, mxbs, poisson=False, protect_rl=False):
    """!
    Function to align meshes.
    @param rijk:  ijk of the ref mesh.
    @param rxbs: xbs of the ref mesh.
    @param mijk: ijk of the other mesh.
    @param mxbs: xbs of the other mesh.
    @param poisson: True for respecting the Poisson constraint.
    @param protect_rl: True to protect ref length.
    @return return new rijk, rxbs, mijk and mxbs.
    """
    # Init
    deltas = (  # rcs
        abs(rxbs[0] - rxbs[1]) / rijk[0],
        abs(rxbs[2] - rxbs[3]) / rijk[1],
        abs(rxbs[4] - rxbs[5]) / rijk[2],
    )
    msgs = list()
    # Are meshes far apart?
    if _is_far(rxbs=rxbs, mxbs=mxbs, deltas=deltas):
        msgs.append("Far apart, alignment not needed")
        return rijk, rxbs, mijk, mxbs, msgs
    else:
        msgs.append("Close enough, alignment needed")
    if protect_rl:
        msgs.append("Ref MESH cell size updated, Object size untouched")  # FIXME
    else:
        msgs.append("Ref MESH size updated, cell size untouched")
    # If mesh sides are close, then snap them
    # otherwise align their meshes
    if abs(rxbs[0] - mxbs[1]) <= deltas[0]:  # -x close?
        msgs.append(f"Close enough at ref x0, snapped")
        mxbs[1] = rxbs[0]
    elif abs(mxbs[0] - rxbs[1]) <= deltas[0]:  # +x close?
        msgs.append(f"Close enough at ref x1, snapped")
        mxbs[0] = rxbs[1]
    else:
        msgs.append("Aligned along x axis")
        _align_along_x(rijk, rxbs, mijk, mxbs, poisson, protect_rl)
    if abs(rxbs[2] - mxbs[3]) <= deltas[1]:  # -y close?
        msgs.append(f"Close enough at ref y0, snapped")
        mxbs[3] = rxbs[2]
    elif abs(mxbs[2] - rxbs[3]) <= deltas[1]:  # +y close?
        msgs.append(f"Close enough at ref y1, snapped")
        mxbs[2] = rxbs[3]
    else:
        msgs.append("Aligned along y axis")
        _align_along_y(rijk, rxbs, mijk, mxbs, poisson, protect_rl)
    if abs(rxbs[4] - mxbs[5]) <= deltas[2]:  # -z close?
        msgs.append(f"Close enough at ref z0, snapped")
        mxbs[5] = rxbs[4]
    elif abs(mxbs[4] - rxbs[5]) <= deltas[2]:  # +z close?
        msgs.append(f"Close enough at ref z1, snapped")
        mxbs[4] = rxbs[5]
    else:
        msgs.append("Aligned along z axis")
        _align_along_z(rijk, rxbs, mijk, mxbs, poisson, protect_rl)
    return rijk, rxbs, mijk, mxbs, msgs


def calc_poisson_ijk(ijk):
    """!
    Get an IJK respecting the Poisson constraint, close to the current one.
    @param ijk: ijk of the mesh.
    @return return new ijk values.
    """
    return ijk[0], _n_for_poisson(ijk[1]), _n_for_poisson(ijk[2])


def calc_cell_sizes(ijk, xbs):
    """!
    Calc MESH cell sizes.
    @param ijk: ijk of the mesh.
    @param xbs: xbs of the mesh.
    @return return the MESH cell sizes.
    """
    return (
        (xbs[1] - xbs[0]) / ijk[0],
        (xbs[3] - xbs[2]) / ijk[1],
        (xbs[5] - xbs[4]) / ijk[2],
    )


def calc_ijk(xbs, desired_cs, poisson):
    """!
    Calc MESH IJK from cell sizes.
    @param xbs: xbs of the mesh.
    @param desired_cs: desired cell sizes.
    @param poisson: True for respecting the Poisson constraint.
    @return return the MESH IJK from cell sizes.
    """
    print(xbs, desired_cs, poisson)  # FIXME
    ijk = (
        round((xbs[1] - xbs[0]) / desired_cs[0]) or 1,
        round((xbs[3] - xbs[2]) / desired_cs[1]) or 1,
        round((xbs[5] - xbs[4]) / desired_cs[2]) or 1,
    )
    if poisson:
        return calc_poisson_ijk(ijk)
    else:
        return ijk


def calc_cell_infos(ijk, xbs):
    """!
    Calc many cell infos: cell ijk goodness, sizes, count and aspect ratio.
    @param ijk: ijk of the mesh.
    @param xbs: xbs of the mesh.
    @return return if cell infos.
    """
    cs = calc_cell_sizes(ijk, xbs)
    has_good_ijk = tuple(ijk) == calc_poisson_ijk(ijk)
    cell_count = ijk[0] * ijk[1] * ijk[2]
    cell_sizes_sorted = sorted(cs)
    try:
        cell_aspect_ratio = max(
            cell_sizes_sorted[2] / cell_sizes_sorted[0],
            cell_sizes_sorted[2] / cell_sizes_sorted[1],
            cell_sizes_sorted[1] / cell_sizes_sorted[0],
        )
    except ZeroDivisionError:
        cell_aspect_ratio = 999.0
    # Return
    return has_good_ijk, cs, cell_count, cell_aspect_ratio


def split_mesh(axis, ijk, xbs):
    """!
    Function to split a mesh.
    @param axis: an integer between 0, 1 and 2 to represent the axis on which to perform the split..
    @param ijk: the ijk of the initial mesh.
    @param xbs: the xbs of the initial mesh.
    @return return the ijk and xbs values of the two new meshes.
    """
    
    def getXBSValue(ax):
        if ax == axis or ax == axis + 3:
            return xbs[axis] + (xbs[axis+3]-xbs[axis])/2
        return xbs[ax]
    
    def getIJKValue(ax):
        if ax == axis:
            return ijk[ax]/2
        return ijk[ax]
    
    if not axis in [0, 1, 2]:
        raise Exception("Invalid axis")

    if ijk[axis] % 2 != 0:
        raise Exception("The number of cells along the selected axis is odd")

    aijk = [getIJKValue(0),
            getIJKValue(1), 
            getIJKValue(2)]
    axbs = [xbs[0], 
            xbs[1], 
            xbs[2], 
            getXBSValue(3), 
            getXBSValue(4), 
            getXBSValue(5)]

    bijk = [getIJKValue(0),
            getIJKValue(1), 
            getIJKValue(2)]
    bxbs = [getXBSValue(0), 
            getXBSValue(1), 
            getXBSValue(2),
            xbs[3], 
            xbs[4], 
            xbs[5]]

    return aijk, axbs, bijk, bxbs


def calc_triangulation(csv_file_path):
    """!
    Function to convert a csv file into a mesh.
    @param csv_file_path: csv file path.
    @return return three array: nodes, connectivity and propeties.
        Nodes: list of nodes that make up the mesh. Each node is made up of three float (x, y and z);
        Connectivity: connectivity list. Each element is made up of three integers;
        Propeties: list of properties of the fourth column of the csv file;
    """

    def get_norm(vector):
        return math.sqrt(vector[0]**2 + vector[1]**2)

    def dot_product(first_point, prev_point, curr_point):
        try:
            if first_point==None:
                return None
            
            if prev_point==None:
                return None
            
            vector_a = [prev_point[0]-first_point[0], prev_point[1]-first_point[1]]
            vector_b = [curr_point[0]-first_point[0], curr_point[1]-first_point[1]]

            return (vector_a[0]*vector_b[0] + vector_a[1]*vector_b[1]) / (get_norm(vector_a) * get_norm(vector_b))
        
        except ZeroDivisionError:
            return None

    def next_step(col, first_point, prev_point, curr_point):
        first_point = first_point if first_point!=None else curr_point
        col = col+1 if col!=None else 0

        # ignoring first two columns
        if col<2:
            return col, first_point, curr_point
        
        # new line
        elif abs(dot_product(first_point, prev_point, curr_point) - 1) >= 0.1:
            return 0, curr_point, None

        # next column
        else:
            return col, first_point, curr_point

    def average(lst):
        return sum(lst)/len(lst) 

    def matrix_has_index(row, col):
        if row<0 or col<0:
            return False
        
        try:
            csv_matrix[row][col]
            return True
        
        except(IndexError):
            return False

    def get_boundary_node(point1, point2):
        return [
            point1["x"] - (point2["x"] - point1["x"])/2,
            point1["y"] - (point2["y"] - point1["y"])/2,
            point1["z"] - (point2["z"] - point1["z"])/2
        ]

    csv_matrix = list()
    nodes = list()
    properties = list()
    connectivity = list()

    with open(csv_file_path, 'r') as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')

        # ignoring csv header
        next(csv_reader)

        col = None
        first_point = None
        prev_point = None

        # Step 1 - csv_matrix generation
        #-----------------------------------------------
        # csv_matrix contains the same information present in the csv file without the header and organized in a matrix
        for row in csv_reader:
            curr_point = [float(row[0]), float(row[1]), float(row[2])]
            col, first_point, prev_point = next_step(col, first_point, prev_point, curr_point)
            dot = dot_product(first_point, prev_point, curr_point)

            if col == 0:
                csv_matrix.append(list())
            
            csv_matrix[-1].append({
                "x": curr_point[0],
                "y": curr_point[1],
                "z": curr_point[2],
                "property": float(row[3]),
            })
        
        # Step 2 - generation of output arrays
        #-----------------------------------------------
        node_count = -1
        for row in range(len(csv_matrix)+1):
            row_len = len(csv_matrix[row]) if row<len(csv_matrix) else len(csv_matrix[-1])

            for col in range(row_len+1):
                node = [None, None, None]
                
                # generation of nodes array
                # each node is calculated as the average of the 4 adjacent face centers (if they exist)
                try:
                    if row==0 or col==0:
                        raise IndexError()
                    
                    node[0] = average([csv_matrix[row-1][col-1]["x"], csv_matrix[row-1][col]["x"], csv_matrix[row][col-1]["x"], csv_matrix[row][col]["x"]])
                    node[1] = average([csv_matrix[row-1][col-1]["y"], csv_matrix[row-1][col]["y"], csv_matrix[row][col-1]["y"], csv_matrix[row][col]["y"]])
                    node[2] = average([csv_matrix[row-1][col-1]["z"], csv_matrix[row-1][col]["z"], csv_matrix[row][col-1]["z"], csv_matrix[row][col]["z"]])

                # generation of boudnary nodes
                # boundary nodes are calculated as a point aligned with two adjacent face centers
                except(IndexError):
                    if matrix_has_index(row, col) and matrix_has_index(row-1, col+1):
                        node = get_boundary_node(csv_matrix[row][col], csv_matrix[row-1][col+1])
                    
                    elif matrix_has_index(row, col) and matrix_has_index(row+1, col-1):
                        node = get_boundary_node(csv_matrix[row][col], csv_matrix[row+1][col-1])
                    
                    elif matrix_has_index(row, col) and matrix_has_index(row+1, col+1):
                        node = get_boundary_node(csv_matrix[row][col], csv_matrix[row+1][col+1])
                    
                    elif matrix_has_index(row, col) and matrix_has_index(row-1, col-1):
                        node = get_boundary_node(csv_matrix[row][col], csv_matrix[row-1][col-1])
                    
                    elif matrix_has_index(row, col-1) and matrix_has_index(row-1, col-2):
                        node = get_boundary_node(csv_matrix[row][col-1], csv_matrix[row-1][col-1])

                    elif matrix_has_index(row, col-1) and matrix_has_index(row+1, col-2):
                        node = get_boundary_node(csv_matrix[row][col-1], csv_matrix[row+1][col-1])
                    
                    elif matrix_has_index(row-1, col) and matrix_has_index(row-2, col+1):
                        node = get_boundary_node(csv_matrix[row-1][col], csv_matrix[row-2][col+1])
                    
                    elif matrix_has_index(row-1, col-1) and matrix_has_index(row-2, col-2):
                        node = get_boundary_node(csv_matrix[row-1][col-1], csv_matrix[row-2][col-2])
                    
                    else:
                        raise ValueError(str(row) + " - " + str(col))

                nodes.append(node)
                node_count += 1

                # generation of connectivity and properties arrays
                if row<len(csv_matrix) and col<len(csv_matrix[row]):
                    node_count_0 = node_count
                    node_count_1 = node_count + 1
                    node_count_2 = node_count + len(csv_matrix[row]) + 1
                    node_count_3 = node_count + len(csv_matrix[row]) + 2

                    connectivity.append([node_count_0, node_count_2, node_count_1])
                    connectivity.append([node_count_1, node_count_2, node_count_3])

                    properties.append(csv_matrix[row][col]["property"])
                    properties.append(csv_matrix[row][col]["property"])
                
    return nodes, connectivity, properties


def test():
    print("Test")
    rijk, rxbs, mijk, mxbs, msgs = align_meshes(
        rijk=[15, 37, 51],
        rxbs=[0, 5, 0, 5, 0, 5],
        mijk=[9, 38, 20],
        mxbs=[0, 5, 0, 5, 5, 10],
        poisson=True,
        protect_rl=True,
    )
    print(rijk, rxbs, mijk, mxbs, msgs)


if __name__ == "__main__":
    test()
