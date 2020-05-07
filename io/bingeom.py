#!/usr/bin/python3

"""!
BlenderFDS, import/export geometry to FDS bingeom binary file format.
"""

# The FDS bingeom file is written from Fortran90 like this:
#      WRITE(731) INTEGER_ONE
#      WRITE(731) N_VERTS,N_FACES,N_SURF_ID,N_VOLUS
#      WRITE(731) VERTS(1:3*N_VERTS)
#      WRITE(731) FACES(1:3*N_FACES)
#      WRITE(731) SURFS(1:N_FACES)
#      WRITE(731) VOLUS(1:4*N_VOLUS)

import struct
import numpy as np


def ob_to_bingeom(context, ob, scale_length, check, world, filepath):
    pass


def _read_record(f, req_dtype, req_dlen):
    """!
    Read a record from an open binary unformatted sequential Fortran90 file.
    @param f: open Python file object in 'rb' mode.
    @param req_dtype: requested type of data in 'int32' or 'float64'.
    @param req_dlen: requested length of the record.
    @return np.array of read data.
    """
    # The start tag is an int32 number (4 bytes) declaring the length of the record in bytes
    tag = struct.unpack("i", f.read(4))[0]
    # Calc the length of the record in numbers, using the byte size of the requested dtype
    dlen = int(tag / np.dtype(req_dtype).itemsize)
    if dlen != req_dlen:
        raise IOError(
            f"Different requested and declared record length: {req_dlen}, {dlen}"
        )
    # Read the record
    data = np.fromfile(f, dtype=req_dtype, count=req_dlen)
    # The end tag should be equal to the start tag
    end_tag = struct.unpack("i", f.read(4))[0]  # end tag, last 4 bytes, int32
    if tag != end_tag:  # check tags
        raise IOError(f"Different start and end record tags: {tag}, {end_tag}")
    # print(f"Read: record tag: {tag} dlen: {len(data)}\ndata: {data}")  # TODO log debug
    return data


def read_bingeom(filepath):
    """!
    Read FDS bingeom file 
    @param filepath: filepath to be read from
    @return n_surf_id as integer, and fds_verts, fds_faces, fds_surfs, fds_volus as np.arrays in FDS flat format
    """
    with open(filepath, "rb") as f:
        one = _read_record(f, req_dtype="int32", req_dlen=1)[0]
        if one != 1:
            raise Exception(f"Mismatched endianness!")
        n_verts, n_faces, n_surf_id, n_volus = _read_record(
            f, req_dtype="int32", req_dlen=4
        )
        fds_verts = _read_record(f, req_dtype="float64", req_dlen=3 * n_verts)
        fds_faces = _read_record(f, req_dtype="int32", req_dlen=3 * n_faces)
        fds_surfs = _read_record(f, req_dtype="int32", req_dlen=n_faces)
        fds_volus = _read_record(f, req_dtype="int32", req_dlen=4 * n_volus)
    return n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus


def _write_record(f, data):
    """!
    Write a record to a binary unformatted sequential Fortran90 file.
    @param f: open Python file object in 'wb' mode.
    @param data: np.array() of data.
    """
    # Calc start and end record tag
    tag = len(data) * data.dtype.itemsize
    # print(f"Write: record tag: {tag} dlen: {len(data)}\ndata: {data}")  # TODO log debug
    # Write start tag, data, and end tag
    f.write(struct.pack("i", tag))
    data.tofile(f)
    f.write(struct.pack("i", tag))


def write_bingeom(n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus, filepath):
    """!
    Write FDS bingeom file.
    @param n_surf_id: number of referred boundary conditions
    @param fds_verts: vertices coordinates in FDS flat format, eg. (x0, y0, z0, x1, y1, ...)
    @param fds_faces: faces connectivity in FDS flat format, eg. (i0, j0, k0, i1, ...)
    @param fds_surfs: boundary condition indexes, eg. (i0, i1, ...)
    @param fds_volus: volumes connectivity in FDS flat format, eg. (i0, j0, k0, w0, i1, ...)
    @param filepath: destination filepath
    """

    # Write
    with open(filepath, "wb") as f:
        _write_record(f, np.array((1,), dtype="int32"))
        ns = np.array(
            (len(fds_verts) // 3, len(fds_faces) // 3, n_surf_id, len(fds_volus) // 4),
            dtype="int32",
        )
        _write_record(f, ns)
        _write_record(f, np.array(fds_verts, dtype="float64"))
        _write_record(f, np.array(fds_faces, dtype="int32"))
        _write_record(f, np.array(fds_surfs, dtype="int32"))
        _write_record(f, np.array(fds_volus, dtype="int32"))


# As command line: read, write and compare the results
if __name__ == "__main__":
    # Check arguments
    import sys

    if len(sys.argv) != 2:
        print("\nUsage: bingeom.py <filepath>")
        sys.exit(2)
    filepath = sys.argv[1]

    print(f"\nReading {filepath}...")
    n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus = read_bingeom(filepath)

    print(f"\nWriting {filepath}.out...\n")
    write_bingeom(
        n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus, filepath + ".out"
    )

    # Compare the binary files
    import os

    os.system(f"xxd {filepath} > {filepath}.hex")  # transform in hex
    os.system(f"xxd {filepath}.out > {filepath}.out.hex")
    print("\nDiff:")
    os.system(f"diff {filepath}.hex {filepath}.out.hex")
    print("\nCompleted!")
