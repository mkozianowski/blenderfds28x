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

import re, os.path, logging

import bpy
from bpy.types import PropertyGroup, UIList, Object, Scene, Material
from bpy.props import (
    BoolProperty,
    FloatProperty,
    IntProperty,
    StringProperty,
    PointerProperty,
    EnumProperty,
    CollectionProperty,
)

from .bl.custom_uilist import get_ops, draw_ops
from .utils import is_iterable

log = logging.getLogger(__name__)

# Blender representations of FDS entities


class BFException(Exception):
    """Exception raised by methods in case of a BF error."""

    def __init__(self, sender, msg):
        self.sender = sender
        self.msg = msg

    def __str__(self):
        sender = self.sender
        try:
            element = sender.element
            name = "{} > {}".format(
                element.name, sender.fds_name or sender.__class__.__name__
            )
        except:
            name = getattr(sender, "name", None) or sender.__class__.__name__
        return "{}: {}".format(name, self.msg)

    def to_fds(self):
        return f"! {self}"


class BFParam:
    """Blender representation of an FDS parameter."""

    label = "No Label"  # Object label
    description = "No description"  # Object description
    enum_id = None  # Unique integer id for EnumProperty
    bf_other = {}  # Other BlenderFDS parameters, eg: {'draw_type': 'WIRE', ...}
    fds_label = None  # FDS label, eg. "OBST", "ID", ...
    fds_default = None  # FDS default value
    bpy_type = None  # type in bpy.types for Blender property, eg. Object
    bpy_idname = None  # idname of related bpy.types Blender property, eg. "bf_id"
    bpy_prop = None  # prop in bpy.props of Blender property, eg. StringProperty
    bpy_default = None  # Blender property default
    bpy_other = {}  # Other optional Blender property parameters, eg. {"min": 3., ...}
    bpy_export = None  # idname of export toggle Blender property
    bpy_export_default = False  # idname of export toggle Blender property

    def __init__(self, element):
        self.element = element

    @classmethod
    def register(cls):
        """Register related Blender properties."""
        log.debug(f"Registering <{cls.label}>")
        if not cls.bpy_type:
            raise Exception(f"No bpy_type in class <{cls}>")
        # Insert fds_default
        if cls.fds_default is not None:
            # ...in description
            cls.description += f"\n[FDS default: {cls.fds_default}]"
            # ...in bpy_default if not present
            if cls.bpy_default is None:
                cls.bpy_default = cls.fds_default
        # Check and create bpy_prop
        if cls.bpy_prop:
            if not cls.bpy_idname or not cls.label or not cls.description:
                raise Exception(f"No bpy_idname, label or description in class <{cls}>")
            if hasattr(cls.bpy_type, cls.bpy_idname):
                log.warning(
                    f"Cannot redefine bpy_idname <{cls.bpy_idname}> in class <{cls}>"
                )
                return
            bpy_other = cls.bpy_other.copy()
            if cls.bpy_default is not None:
                bpy_other["default"] = cls.bpy_default
            log.debug(f"Setting <{cls.bpy_idname}> Blender property")
            setattr(
                cls.bpy_type,
                cls.bpy_idname,
                cls.bpy_prop(name=cls.label, description=cls.description, **bpy_other),
            )
        # Check and create bpy_export
        if cls.bpy_export and not hasattr(cls.bpy_type, cls.bpy_export):
            log.debug(f"Setting <{cls.bpy_export}> Blender property")
            setattr(
                cls.bpy_type,
                cls.bpy_export,
                BoolProperty(
                    name=f"Export {cls.label}",
                    description=f"Set if {cls.label} shall be exported to FDS",
                    default=cls.bpy_export_default,
                ),
            )

    @classmethod
    def unregister(cls):
        """Unregister related Blender properties."""
        if cls.bpy_prop:
            delattr(cls.bpy_type, cls.bpy_idname)

    @property
    def value(self) -> "any":
        """Get value."""
        if not self.bpy_idname:
            raise Exception(f"value() not implemented in class <{self.__class__}>")
        return getattr(self.element, self.bpy_idname)

    def set_value(self, context, value=None):  # FIXME Test
        """Set value."""
        # Do not raise BFException here. Check is performed by UI, do not add overhead!
        if self.bpy_idname:
            if value is not None:
                setattr(self.element, self.bpy_idname, value)
            else:
                setattr(self.element, self.bpy_idname, self.bpy_default)

    @property
    def exported(self) -> "bool":
        """Get if self is exported."""
        # Check if empty
        value = self.value
        if value is None or value == "":
            return False
        # Check if default
        d = self.fds_default
        if d is not None:
            if isinstance(value, float):  # floats comparison
                return value > d + 1e-6 or value < d - 1e-6
            elif value == d:  # other comparison
                return False
        # Check if bpy_export
        return bool(getattr(self.element, str(self.bpy_export), True))

    def set_exported(self, context, value=None):  # FIXME Test
        """Set if self is exported."""
        if self.bpy_export:
            if value is not None:
                setattr(self.element, self.bpy_export, value)
            else:
                setattr(self.element, self.bpy_export, self.bpy_export_default)

    def check(self, context):
        """Check self validity."""
        pass

    def draw(self, context, layout) -> "layout":
        """Draw self UI on layout."""
        col = layout.column()
        active = bool(self.exported)
        col.active = active
        if active:
            try:
                self.check(context)
            except BFException:
                col.alert = True
        if self.bpy_idname:
            if self.bpy_export:
                row = col.row()
                row.prop(self.element, self.bpy_idname, text=self.label)
                row.prop(self.element, self.bpy_export, text="")
            else:
                col.prop(self.element, self.bpy_idname, text=self.label)
        else:
            col.label(
                text=f"draw() not implemented in class '{self.__class__}'", icon="ERROR"
            )
        return col

    def to_fds(self, context) -> "str or (str, msg) or None":
        """Get FDS exported string and msg."""
        # Export requested and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # If value is not an iterable, then put it in a tuple
        value = self.value
        if not is_iterable(value):
            values = tuple((value,))
        else:
            values = value
        # Use common format routine
        return FDSParam._format(
            label=self.fds_label,
            values=values,
            precision=self.bpy_other.get("precision", 3),
        )

    def from_fds(self, context, value):  # FIXME test
        """Set parameter value from value in FDS notation, on error raise BFException."""
        try:
            self.set_value(context, value)
        except Exception as err:
            raise BFException(self, f"Error importing '{value}' value: {str(err)}")
        self.set_exported(context, True)


class BFNamelist(BFParam):
    """Blender representation of an FDS namelist group."""

    # tuple of managed BFParam classes
    # used for auto drawing panel, exporting
    bf_params = tuple()
    # dict of BFParam classes by fds_label (autogenerated)
    # used for importing
    bf_params_by_fds = dict()

    # Allowed indexes for specific parameters
    bf_xb_idxs, bf_xyz_idxs, bf_pb_idxs, bf_id_suffix_idxs = None, None, None, None

    @classmethod
    def register(cls, *args, **kwargs):
        super().register(*args, **kwargs)
        # Create automatic properties
        if cls.bf_params:
            bf_params_by_fds = dict(
                (p.fds_label, p) for p in cls.bf_params if p.fds_label
            )

    def draw(self, context, layout) -> "layout":
        """Draw self UI on layout."""
        el = self.element
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        col.active = self.exported
        for p in self.bf_params:
            p(el).draw(context, col)
        return col

    @property
    def exported(self) -> "bool":
        """Get if self is exported."""
        return bool(getattr(self.element, str(self.bpy_export), True))

    def to_fds(self, context) -> "str or None":
        """Get FDS exported string and msg."""
        if not self.exported:
            return
        self.check(context)
        # Init
        param_strings, multiparam_strings, msgs = list(), None, list()
        # Get param_strings and msgs
        for p in self.bf_params:
            result = p(self.element).to_fds(context)
            if result:
                if isinstance(result, str):  # no msgs
                    result and param_strings.append(result)  # a not empty string
                else:
                    result[0] and param_strings.append(result[0])  # a not empty string
                    result[1] and msgs.append(result[1])  # a not empty msg
        # Search multiparam_strings
        for i, p in enumerate(param_strings):
            if not isinstance(p, str):
                multiparam_strings = param_strings.pop(i)  # pop multiparam
                param_strings.pop(0)  # rm ID too, always the first param
                break
        # Use common format routine
        if self.bpy_type == Object:
            max_cols = 130
        else:
            max_cols = 0
        return FDSNamelist._format(
            label=self.fds_label,
            param_strings=param_strings,
            multiparam_strings=multiparam_strings,
            msgs=msgs,
            max_cols=max_cols,
        )

    def from_fds(self, context, fds_params):  # FIXME develop
        """Set namelist parameter values from value in FDS notation, on error raise BFException."""
        self.set_exported(context, True)


class BFParamStr(BFParam):
    """Helper for FDS string parameter."""

    bpy_prop = StringProperty

    def check(self, context):
        value = self.value
        if value and not re.match(r"^[\w\-. ]+$", value):
            raise BFException(self, "Special characters not allowed in string")


class BFParamFYI(BFParamStr):
    """Helper for FDS FYI parameter."""

    label = "FYI"
    description = "For your information"
    fds_label = "FYI"
    bpy_idname = "bf_fyi"
    bpy_prop = StringProperty
    bpy_other = {"maxlen": 128}

    def draw(self, context, layout):
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        if self.bpy_idname:
            col.prop(self.element, self.bpy_idname, text="", icon="INFO")
        return col


class BFParamOthers(BFParam):
    """Helper for FDS other parameters."""

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_type = None  # eg. Object
    bpy_idname = "bf_others"

    bpy_pg = None  # PropertyGroup, eg. WM_PG_bf_others
    bpy_ul = None  # UIList, eg. WM_UL_bf_others_items

    element = None

    @classmethod
    def register(cls):
        # Register index and collection
        cls._idx_idname = f"{cls.bpy_idname}_idx"
        prop = IntProperty(name="Index", default=0)
        setattr(cls.bpy_type, cls._idx_idname, prop)
        prop = CollectionProperty(
            name=cls.label, description=cls.description, type=cls.bpy_pg
        )
        setattr(cls.bpy_type, cls.bpy_idname, prop)
        # Register operators
        cls._op_idname = f"{cls.bpy_type.__name__.lower()}.{cls.bpy_idname}"
        cls._ops = get_ops(cls.bpy_type, cls.bpy_idname, cls._idx_idname)
        for op in cls._ops:
            bpy.utils.register_class(op)

    @classmethod
    def unregister(cls):
        # Unregister operators
        for op in cls._ops:
            bpy.utils.unregister_class(op)
        # Unregister index and collection
        delattr(cls.bpy_type, cls._idx_idname)
        delattr(cls.bpy_type, cls.bpy_idname)

    def draw(self, context, layout):
        layout.label(text=self.label)
        row = layout.row()
        row.template_list(
            self.bpy_ul.__name__,
            "",
            self.element,
            self.bpy_idname,
            self.element,
            self._idx_idname,
            rows=3,
        )
        draw_ops(context, row, self._op_idname)

    def to_fds(self, context):
        self.check(context)
        ccollection = getattr(self.element, self.bpy_idname)
        if ccollection:
            return " ".join(p.name for p in ccollection if p.bf_export and p.name)


# Python representations of FDS entities


class FDSParam:
    """Interface between FDS namelist parameter and its Python representation.
    
    label: namelist parameter label
    values: list of parameter values
    precision: float precision, number of decimal digits
    """

    _re_precision = r"\.([0-9]*)"  # decimal positions

    _scan_precision = re.compile(
        _re_precision, re.VERBOSE | re.DOTALL | re.IGNORECASE
    )  # no MULTILINE, so that $ is the end of the file FIXME

    def __init__(self, label, values=None, precision=None):
        self.label = label
        if values is not None:
            self.values = values
        else:
            self.values = list()
        if precision is not None:
            self.precision = precision
        else:
            self.precision = 3

    def __str__(self):
        return self.to_fds()

    @staticmethod
    def _format(label, values, precision=3):
        if not values:
            raise ValueError(f"Empty value for parameter <{label}>")
        # Check first element of the iterable and choose formatting
        v0 = values[0]
        if isinstance(v0, float):
            v_string = ",".join(f"{v:.{precision}f}" for v in values)
        elif isinstance(v0, int):
            v_string = ",".join(str(v) for v in values)
        elif isinstance(v0, str):
            v_string = ",".join("'" in v and f'"{v}"' or f"'{v}'" for v in values)
        elif isinstance(v0, bool):
            v_string = ",".join(v and "T" or "F" for v in values)
        else:
            raise ValueError(f"Unknown value type for parameter <{label}>")
        return "=".join((label, v_string))

    def to_fds(self):
        """Export to fds string."""
        return self._format(
            label=self.label, values=self.values, precision=self.precision
        )

    def from_fds(self, f90_values):
        """Import from f90_values string, eg. "2.34, 1.23, 3.44"    
        """
        f90_values += ","  # always return a tuple, eg. "34" -> "34,"
        # Translate F90 booleans
        if f90_values[0] == ".":
            f90_values = (
                f90_values.upper().replace(".TRUE.", "True").replace(".FALSE.", "False")
            )
        elif f90_values[0] in ("T", "F"):
            f90_values = f90_values.upper().replace("T", "True").replace("F", "False")
        # Python evaluation of F90 value
        try:
            self.values = eval(f90_values)
        except Exception:
            raise SyntaxError(
                f"Parsing error in parameter <{self.label}={f90_values} ... />"
            )
        # Get precision from the first f90 float value
        if isinstance(self.values[0], float):
            match = re.search(self._re_precision, f90_values)
            if match:
                self.precision = len(match.groups()[0])


class FDSNamelist:
    """Interface between FDS Namelist and its Python representation.
    
    label: namelist group label, string
    fds_params: list of FDSParam instances
    max_cols: max columns of formatted output
    """

    def __init__(self, label, fds_params=None, max_cols=6):
        self.label = label
        if fds_params is not None:
            self.fds_params = fds_params
        else:
            self.fds_params = list()  # list of FDSParam
        self.max_cols = max_cols

    def __str__(self):
        return self.to_fds()

    @staticmethod
    def _format(
        label, param_strings=None, multiparam_strings=None, msgs=None, max_cols=130
    ):
        """Format FDS namelist to string

        label: namelist group label, string
        param_strings: list of parameter strings, eg. ["ID='Test'", ...]
        multiparam_strings: list of parameter strings that are repeated, eg. ["ID='T_1' XB=1,2,3,4,5,6", ...]
        msgs: list of comment msgs, eg. ["Comment 1", "Comment 2", ...]
        max_cols: max columns of formatted output
        """
        lines = list()
        # Include message lines
        if msgs:
            lines.extend((f"! {m}" for m in msgs))
        # Create nls_chunks
        nls_chunks = list()
        if multiparam_strings:
            for mp in multiparam_strings:
                chunks = list()
                chunks.append(mp)
                chunks.extend(param_strings)
                nls_chunks.append(chunks)
        else:
            chunks = list()
            chunks.extend(param_strings)
            nls_chunks.append(chunks)
        # Create nls_strings
        max_len = max_cols - 5  # len("&OBST") == 5
        for chunks in nls_chunks:
            cur_nl = ["&", label]
            cur_len = 5
            chunks.reverse()  # for efficient pop()
            while chunks:
                chunk = chunks.pop()
                l = 1 + len(chunk)  # separator + chunk
                if cur_len == 5 or cur_len + l <= max_len:  # first or not too long
                    cur_nl.extend((" ", chunk))
                    cur_len += l
                else:  # next line
                    cur_nl.extend(("\n      ", chunk))
                    cur_len = 5 + l
            cur_nl.append(" /")
            lines.append("".join(cur_nl))
        return "\n".join(lines)

    def to_fds(self):
        """Export to fds string."""
        return self._format(
            label=self.label,
            param_strings=(p.to_fds() for p in self.fds_params),
            max_cols=self.max_cols,
        )

    _re_label = r"([A-Z][A-Z0-9_\(\):,]+?)"  # param label w indexes
    _re_space = r"[\s\t]*"  # zero or more spaces
    _re_sep = r"[,\s\t]+"  # one or more separators
    _re_end = r"[,\s\t]*/"
    _re_values = (
        r"(.+?)"  # one or more of any char, not greedy
        + r"(?="  # end previous match when
        + _re_sep
        + _re_label  # either a new label
        + _re_space
        + "="  # followed by an equal sign
        + "|"  # or
        + _re_end  # the end of the namelist
        + ")"
    )
    _re_param = (
        _re_label + _re_space + "=" + _re_space + _re_values
    )  # groups: label, vals

    _scan = re.compile(
        _re_param, re.VERBOSE | re.DOTALL | re.IGNORECASE
    )  # no MULTILINE, so that $ is the end of the file FIXME

    def from_fds(self, f90_params):
        """Import from f90_params string, eg. "ID='Test' PROP=2.34, 1.23, 3.44"    
        """
        for match in re.finditer(self._scan, f90_params):
            label, f90_values, following_label = match.groups()
            p = FDSParam(label=label)
            try:
                p.from_fds(f90_values=f90_values)
            except SyntaxError as err:
                log.warning(f"<{label}> <{err}>")  # FIXME error msg
            else:
                self.fds_params.append(p)
            if following_label is None:
                break


class FDSCase:
    """Interface between FDS case and its Python representation.

    fds_namelists: list of FDSNamelist instances
    """

    def __init__(self, fds_namelists=None):
        if fds_namelists:
            self.fds_namelists = fds_namelists
        else:
            self.fds_namelists = list()
        self._index = len(self.fds_namelists)

    def __iter__(self):
        return self

    def __next__(self):
        if self._index == len(self.fds_namelists) - 1:
            raise StopIteration
        self._index += 1
        return self.fds_namelists[self._index]

    def __getitem__(self, i):
        return FDSCase(self.fds_namelists[i])

    def __str__(self):
        return self.to_fds()

    def to_fds(self):
        """Export to fds string."""
        return "\n".join(n.to_fds() for n in self.fds_namelists)

    _scan = re.compile(
        r"""
        (?:^&)             # & at the beginning
        ([A-Z]+[A-Z0-9]*)  # namelist label
    """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE | re.MULTILINE,
    )  # MULTILINE, so that ^ is the beginning of each line

    def from_fds(self, f90_namelists):
        """Import from f90_namelists string, eg. "&OBST ... /\n&DEVC ... /" 
        """
        self.fds_namelists = list()
        for match in re.finditer(self._scan, f90_namelists):
            nl = FDSNamelist(label=match.groups()[0])
            nl.from_fds(f90_params=f90_namelists[match.end() :])
            self.fds_namelists.append(nl)


if __name__ == "__main__":
    import sys

    if not sys.argv:
        exit()
    with open(sys.argv[1], "r") as f:
        fdsfile = f.read()
    c = FDSCase()
    c.from_fds(fdsfile)
    print(c)
