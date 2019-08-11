# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

import re


def is_iterable(var):
    """Check if var is iterable or not

    >>> is_iterable("hello"), is_iterable((1,2,3)), is_iterable({1,2,3})
    (False, True, True)
    """
    # A str is iterable in Py... not what I want
    if isinstance(var, str):
        return False
    # Let's try and fail nicely
    try:
        for _ in var:
            break
    except TypeError:
        return False
    return True


# Write to file


def is_writable(filepath):
    """Check if filepath is writable"""
    return write_to_file(filepath, "! Test: this file is writable")


def write_to_file(filepath, text_file):
    """Write text_file to filepath"""
    if text_file is None:
        text_file = str()
    try:
        with open(filepath, "w", encoding="utf8", errors="ignore") as out_file:
            out_file.write(text_file)
        return True
    except IOError:
        return False

