#!/usr/bin/python3

"""!
BlenderFDS, import/export terrain (DEM and landuse).
"""


import csv
import math


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
        return math.sqrt(vector[0] ** 2 + vector[1] ** 2)

    def dot_product(first_point, prev_point, curr_point):
        try:
            if first_point == None:
                return None

            if prev_point == None:
                return None

            vector_a = [prev_point[0] - first_point[0], prev_point[1] - first_point[1]]
            vector_b = [curr_point[0] - first_point[0], curr_point[1] - first_point[1]]

            return (vector_a[0] * vector_b[0] + vector_a[1] * vector_b[1]) / (
                get_norm(vector_a) * get_norm(vector_b)
            )

        except ZeroDivisionError:
            return None

    def next_step(col, first_point, prev_point, curr_point):
        first_point = first_point if first_point != None else curr_point
        col = col + 1 if col != None else 0

        # ignoring first two columns
        if col < 2:
            return col, first_point, curr_point

        # new line
        elif abs(dot_product(first_point, prev_point, curr_point) - 1) >= 0.1:
            return 0, curr_point, None

        # next column
        else:
            return col, first_point, curr_point

    def average(lst):
        return sum(lst) / len(lst)

    def matrix_has_index(row, col):
        if row < 0 or col < 0:
            return False

        try:
            csv_matrix[row][col]
            return True

        except (IndexError):
            return False

    def get_boundary_node(point1, point2):
        return [
            point1["x"] - (point2["x"] - point1["x"]) / 2,
            point1["y"] - (point2["y"] - point1["y"]) / 2,
            point1["z"] - (point2["z"] - point1["z"]) / 2,
        ]

    csv_matrix = list()
    nodes = list()
    properties = list()
    connectivity = list()

    with open(csv_file_path, "r") as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=",")

        # ignoring csv header
        next(csv_reader)

        col = None
        first_point = None
        prev_point = None

        # Step 1 - csv_matrix generation
        # -----------------------------------------------
        # csv_matrix contains the same information present in the csv file without the header and organized in a matrix
        for row in csv_reader:
            curr_point = [float(row[0]), float(row[1]), float(row[2])]
            col, first_point, prev_point = next_step(
                col, first_point, prev_point, curr_point
            )
            dot = dot_product(first_point, prev_point, curr_point)

            if col == 0:
                csv_matrix.append(list())

            csv_matrix[-1].append(
                {
                    "x": curr_point[0],
                    "y": curr_point[1],
                    "z": curr_point[2],
                    "property": float(row[3]),
                }
            )

        # Step 2 - generation of output arrays
        # -----------------------------------------------
        node_count = -1
        for row in range(len(csv_matrix) + 1):
            row_len = (
                len(csv_matrix[row]) if row < len(csv_matrix) else len(csv_matrix[-1])
            )

            for col in range(row_len + 1):
                node = [None, None, None]

                # generation of nodes array
                # each node is calculated as the average of the 4 adjacent face centers (if they exist)
                try:
                    if row == 0 or col == 0:
                        raise IndexError()

                    node[0] = average(
                        [
                            csv_matrix[row - 1][col - 1]["x"],
                            csv_matrix[row - 1][col]["x"],
                            csv_matrix[row][col - 1]["x"],
                            csv_matrix[row][col]["x"],
                        ]
                    )
                    node[1] = average(
                        [
                            csv_matrix[row - 1][col - 1]["y"],
                            csv_matrix[row - 1][col]["y"],
                            csv_matrix[row][col - 1]["y"],
                            csv_matrix[row][col]["y"],
                        ]
                    )
                    node[2] = average(
                        [
                            csv_matrix[row - 1][col - 1]["z"],
                            csv_matrix[row - 1][col]["z"],
                            csv_matrix[row][col - 1]["z"],
                            csv_matrix[row][col]["z"],
                        ]
                    )

                # generation of boudnary nodes
                # boundary nodes are calculated as a point aligned with two adjacent face centers
                except (IndexError):

                    # management of row and col boundaries
                    if row == 0 and (col != 0 and col < row_len):
                        node = get_boundary_node(
                            csv_matrix[row][col], csv_matrix[row + 1][col]
                        )

                    elif (row != 0 and row < len(csv_matrix)) and col == 0:
                        node = get_boundary_node(
                            csv_matrix[row][col], csv_matrix[row][col + 1]
                        )

                    elif row == len(csv_matrix) and (col != 0 and col < row_len):
                        node = get_boundary_node(
                            csv_matrix[row - 1][col], csv_matrix[row - 2][col]
                        )

                    elif (row != 0 and row < len(csv_matrix)) and col == row_len:
                        node = get_boundary_node(
                            csv_matrix[row][col - 1], csv_matrix[row][col - 2]
                        )

                    # management of the four corners
                    elif (row == 0) and (col == 0):
                        node = get_boundary_node(
                            csv_matrix[row][col], csv_matrix[row + 1][col + 1]
                        )

                    elif (row == len(csv_matrix)) and (col == 0):
                        node = get_boundary_node(
                            csv_matrix[row - 1][col], csv_matrix[row - 2][col + 1]
                        )

                    elif (row == 0) and (col == row_len):
                        node = get_boundary_node(
                            csv_matrix[row][col - 1], csv_matrix[row + 1][col - 2]
                        )

                    elif (row == len(csv_matrix)) and (col == row_len):
                        node = get_boundary_node(
                            csv_matrix[row - 1][col - 1], csv_matrix[row - 2][col - 2]
                        )

                    else:
                        raise ValueError(str(row) + " - " + str(col))

                nodes.append(node)
                node_count += 1

                # generation of connectivity and properties arrays
                if row < len(csv_matrix) and col < len(csv_matrix[row]):
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


# if __name__ == "__main__":
#     test()


# TODO @GISSI -> Verification of calc_triangulation using an STL file as output
def test2(filepath):
    print("Test2:", filepath)
    [nodes, connectivity, properties] = calc_triangulation(filepath)
    # print(nodes)  # [[1.,2.,3.],[...],...]
    # print(connectivity)  # [[1,2,3], [...], ...]
    # print(properties)  # [1,2,3,4, ...]
    c = 471000.0, 4915200.0, 0.0
    verts_str = "".join(
        (f"{v[0]-c[0]:.3f},{v[1]-c[1]:.3f},{v[2]-c[2]:.3f}," for v in nodes)
    )
    faces_str = "".join(
        (
            f"{vi[0]},{vi[1]},{vi[2]},{int(properties[fi])},"
            for fi, vi in enumerate(connectivity)
        )
    )

    f = open(f"{filepath}.fds", "w")
    f.write("&MISC ORIGIN_LAT=8.635897 ORIGIN_LON=44.389503 /\n")
    f.write("&DEVC ID='FIRE_START' XYZ=1143.6,1027.7,192.0 QUANTITY='TEMPERATURE' /\n")
    f.write("&SURF ID='P01-A05' RGB=249,197,92 /\n")
    f.write("&SURF ID='P02-A04' RGB=254,193,119 /\n")
    f.write("&SURF ID='P03-Barren' RGB=133,153,156 /\n")
    f.write("&SURF ID='P04-A03' RGB=236,212,99 /\n")
    f.write("&SURF ID='P05-A10' RGB=114,154,85 /\n")
    f.write("&SURF ID='P06-A01' RGB=255,254,212 /\n")
    f.write("&SURF ID='P07-A08' RGB=229,253,214 /\n")
    f.write(
        f"&GEOM ID='Cogoleto Fire' SURF_ID='P01-A05','P02-A04','P03-Barren','P04-A03','P05-A10','P06-A01','P07-A08'\nVERTS={verts_str}\nFACES={faces_str} /\n"
    )
    f.close()


    c = 471000.0, 4915200.0, 0.0
    verts_str = "\n".join(
        (f"v {v[0]-c[0]:.3f} {v[1]-c[1]:.3f} {v[2]-c[2]:.3f} " for v in nodes)
    )
    faces_str = "\n".join(
        (
            f"usemtl P{int(properties[fi])}\nf {vi[0]+1} {vi[1]+1} {vi[2]+1}"
            for fi, vi in enumerate(connectivity)
        )
    )


    f = open(f"{filepath[:-4]}.obj", "w")
    f.write(f"mtllib {filepath[:-4]}.mtl\n")
    f.write("\n")
    f.write(f"o terrain\n")
    f.write("\n")
    f.write(verts_str)
    f.write("\n")
    f.write(faces_str)
    f.close()


    exit()

    f = open("output.stl", "w")
    f.write("solid\n")

    center = 471000, 4915200, 0.0

    for face in connectivity:
        f.write("    facet normal 0 0 1\n")
        f.write("        outer loop\n")

        for i in range(3):
            node = nodes[face[i]]
            f.write(
                "            vertex "
                + str(node[0] - center[0])
                + " "
                + str(node[1] - center[1])
                + " "
                + str(node[2] - center[2])
                + "\n"
            )

    f.write("        endloop\n")
    f.write("    endfacet\n")

    f.write("endsolid\n")
    f.close()


if __name__ == "__main__":

    # Check arguments
    import sys

    if len(sys.argv) != 2:
        print("\nUsage: terrain.py <filepath>")
        sys.exit(2)
    filepath = sys.argv[1]

    # test(filepath)
    test2(filepath)
