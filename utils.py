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

import os


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


# File operations


def is_file(filepath):
    """Check if filepath exists and is a file."""
    return os.path.exists(filepath) and os.path.isfile(filepath)


def is_writable(filepath):
    """Check if filepath is writable."""
    try:
        write_to_file(filepath, "! Test")
    except IOError:
        return False
    # FIXME rm written test file
    return True


def write_to_file(filepath, text):
    """Write text_file to filepath"""
    if text is None:
        text = str()
    with open(filepath, "w", encoding="utf8", errors="ignore") as f:
        f.write(text)


def read_from_file(filepath):
    encodings = [
        ("utf8", "ignore"),
        ("windows-1252", None),
        ("utf8", None),
    ]  # Last tested first
    while encodings:
        e = encodings.pop()
        try:
            with open(filepath, "r", encoding=e[0], errors=e[1]) as f:
                return f.read()
        except UnicodeDecodeError:
            pass
        except Exception as err:
            raise IOError(f"File not readable: {err}")
    raise IOError("File not readable, unknown text encoding")

