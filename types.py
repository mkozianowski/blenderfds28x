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

if __name__ != "__main__":  # FIXME
    from .bl import custom_uilist
    from .utils import is_iterable

log = logging.getLogger(__name__)

# BF specific exception


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


# Blender representations of FDS entities


class BFParam:
    """Blender representation of an FDS parameter."""

    label = "No Label"  # Object label
    description = "No description"  # Object description
    enum_id = None  # Unique integer id for EnumProperty
    bf_other = {}  # Other BlenderFDS parameters, eg: {'draw_type': 'WIRE', ...}
    bf_params = tuple()  # tuple of sub params of type BFParam
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
            raise Exception(f"Not implemented in class <{self.__class__}>")
        return getattr(self.element, self.bpy_idname)

    def set_value(self, context, value=None):
        """Set value. If value is None, set defaul"""
        if self.bpy_idname:
            if value is not None:
                setattr(self.element, self.bpy_idname, value)
                return
            if self.bpy_default:
                setattr(self.element, self.bpy_idname, self.bpy_default)
                return
        raise BFException(self, f"Error while setting value <{value}>")

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
        if not self.bpy_idname:
            return
        col = layout.column()
        active = bool(self.exported)
        col.active = active
        if active:
            try:
                self.check(context)
            except BFException:
                col.alert = True
        if self.bpy_export:
            row = col.row()
            row.prop(self.element, self.bpy_idname, text=self.label)
            row.prop(self.element, self.bpy_export, text="")
        else:
            col.prop(self.element, self.bpy_idname, text=self.label)
        return col

    def to_fds_param(self, context) -> "None, FDSParam, or ((FDSParam, ...), ...)":
        """Return the FDSParam Python representation."""
        # Get if exported and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # If value is not an iterable, then put it in a tuple
        value = self.value
        if not is_iterable(value):
            values = tuple((value,))
        else:
            values = value
        # Return
        return FDSParam(
            label=self.fds_label,
            values=values,
            precision=self.bpy_other.get("precision", 3),
        )

    def to_fds(self, context) -> "None or str":
        """Return the FDS str representation."""
        fds_param = self.to_fds_param(context)
        return fds_param and str(fds_param)

    def from_fds(self, context, value):
        """Set parameter value from value in FDS notation, on error raise BFException."""
        log.debug(f"{self} {value}")
        if len(value) == 1:  # FIXME FDSParam.values is always a list
            value = value[0]
        try:
            self.set_value(context, value)
        except Exception as err:
            raise BFException(self, f"Error importing <{value}> value, {str(err)}")
        self.set_exported(context, True)


class BFParamXB(BFParam):
    """Helper for FDS XB parameter."""

    bf_xb_idxs = (0, 1, 2, 3, 4)  # allowed xb idxs


class BFParamXYZ(BFParam):
    """Helper for FDS XYZ parameter."""

    bf_xyz_idxs = (0, 1)  # allowed xyz idxs


class BFParamPB(BFParam):
    """Helper for FDS PB parameter."""

    bf_pb_idxs = (0,)  # allowed pb idxs


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


class BFParamOther(BFParam):
    """Helper for FDS other parameters."""

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_type = Object
    bpy_idname = "bf_other"

    bpy_pg = None  # PropertyGroup, eg. WM_PG_bf_other
    bpy_ul = None  # UIList, eg. WM_UL_bf_other_items

    @classmethod
    def register(cls):
        cls._ops = custom_uilist.register_collection(
            bpy_type=cls.bpy_type,
            bpy_idname=cls.bpy_idname,
            name=cls.label,
            bpy_pg=cls.bpy_pg,
            description=cls.description,
        )

    @classmethod
    def unregister(cls):
        custom_uilist.unregister_collection(
            bpy_type=cls.bpy_type, bpy_idname=cls.bpy_idname, ops=cls._ops
        )

    def set_value(self, context, value):  # FIXME test
        collection = getattr(self.element, self.bpy_idname)
        item = collection.add()
        item.name, item.bf_export = value, True

    def draw(self, context, layout):
        custom_uilist.draw_collection(
            element=self.element,
            context=context,
            layout=layout,
            bpy_type=self.bpy_type,
            bpy_idname=self.bpy_idname,
            name=self.label,
            bpy_ul=self.bpy_ul,
        )

    def to_fds_param(self, context):
        self.check(context)
        coll = getattr(self.element, self.bpy_idname)
        if coll:
            return tuple(
                (FDSParam(label=p.name) for p in coll if p.bf_export and p.name)
            )  # many


# BFNamelist is a special BFParam


class BFNamelist(BFParam):
    """Blender representation of an FDS namelist group."""

    max_len = 80  # max columns when exporting FIXME

    def __init__(self, element):
        self.element = element
        self.bf_params = tuple(p(element) for p in self.bf_params)  # instantiate

    @classmethod
    def register(cls):
        super().register()
        cls._bf_param_xb_idx = None  # index of param of type BFParamXB
        cls._bf_param_xyz_idx = None  # ... of type BFParamXYZ
        cls._bf_param_pb_idx = None  # ... of type BFParamPB
        cls._bf_param_other_idx = None  # ... of type BFParamOther
        for i, p in enumerate(cls.bf_params):
            if issubclass(p, BFParamXB):
                cls._bf_param_xb_idx = i
            elif issubclass(p, BFParamXYZ):
                cls._bf_param_xyz_idx = i
            elif issubclass(p, BFParamPB):
                cls._bf_param_pb_idx = i
            elif issubclass(p, BFParamOther):
                cls._bf_param_other_idx = i

    @property
    def bf_param_xb(self):
        if self._bf_param_xb_idx is not None:
            return self.bf_params[self._bf_param_xb_idx]

    @property
    def bf_param_xyz(self):
        if self._bf_param_xyz_idx is not None:
            return self.bf_params[self._bf_param_xyz_idx]

    @property
    def bf_param_pb(self):
        if self._bf_param_pb_idx is not None:
            return self.bf_params[self._bf_param_pb_idx]

    @property
    def bf_param_other(self):
        if self._bf_param_other_idx is not None:
            return self.bf_params[self._bf_param_other_idx]

    def draw(self, context, layout) -> "layout":
        """Draw self UI on layout."""
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        col.active = self.exported
        for p in self.bf_params:
            p.draw(context, col)
        return col

    @property
    def exported(self) -> "bool":
        """Get if self is exported."""
        return bool(getattr(self.element, str(self.bpy_export), True))

    def get_bf_param_by_fds_label(self, fds_label) -> "None or BFParam":
        """Get my bf_param by FDS label."""
        for p in self.bf_params:
            if p.fds_label == fds_label:
                return p

    def to_fds_namelist(self, context) -> "None or FDSNamelist":
        """Return the FDSNamelist Python representation."""
        # Get if exported and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # Assemble from bf_params
        return FDSNamelist(
            label=self.fds_label,
            fds_params=list(p.to_fds_param(context) for p in self.bf_params if p),
            max_len=self.max_len,
        )

    def to_fds(self, context) -> "None or str":
        """Return the FDS str representation."""
        fds_namelist = self.to_fds_namelist(context)
        return fds_namelist and str(fds_namelist)

    def from_fds(self, context, fds_params):
        """Set namelist parameter values from list of FDSParam, on error raise BFException."""
        for p in fds_params:
            bf_param = self.get_bf_param_by_fds_label(
                p.label
            )  # FIXME precomputed dict is faster
            if not bf_param:
                bf_param_other = self.bf_param_other
                if bf_param_other:
                    bf_param_other.set_value(context, value=str(p))
                else:
                    raise BFException(self, f"Value {p} is not managed")
                continue
            bf_param.from_fds(context, p.values)
        self.set_exported(context, True)


# Python representations of FDS entities


class FDSParam:
    """Python datastructure for FDS namelist parameter.
    
    label: namelist parameter label
    values: list of parameter values of type float, int, str, bool
    precision: float precision, number of decimal digits
    msg: comment msg
    """

    def __init__(self, label, values=None, precision=3, msg=None):
        self.label = label
        self.values = values or list()
        self.precision = precision
        self.msg = msg

    def __str__(self):
        if not self.values:
            return self.label
        # Check first element of the iterable and choose formatting
        v0 = self.values[0]
        if isinstance(v0, float):
            v_string = ",".join(f"{v:.{self.precision}f}" for v in self.values)
        elif isinstance(v0, str):
            v_string = ",".join("'" in v and f'"{v}"' or f"'{v}'" for v in self.values)
        elif isinstance(v0, bool):  # always before int
            v_string = ",".join(v and "T" or "F" for v in self.values)
        elif isinstance(v0, int):
            v_string = ",".join(str(v) for v in self.values)
        else:
            raise ValueError(f"Unknown value type for parameter <{self.label}>")
        return "=".join((self.label, v_string))

    _re_precision = r"\.([0-9]*)"  # decimal positions

    _scan_precision = re.compile(_re_precision, re.VERBOSE | re.DOTALL | re.IGNORECASE)

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
    """Python datastructure for FDS namelist.
    
    label: namelist group label, string
    fds_params: list of FDSParam instances.
        Can contain one and only one list of lists of FDSParam instances
        to represent multiple params: (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...)
    msg: comment msg
    max_len: max columns of formatted output
    """

    def __init__(self, label, fds_params=None, msg=None, max_len=80):
        self.label = label
        self.fds_params = fds_params or list()
        self.msg = msg
        self.max_len = max_len

    def __str__(self):
        ps, mps, msgs = list(), list(), list((self.msg,))
        # Select parameters ps, multi parameters mps, messages msgs
        for p in self.fds_params:
            if not p:  # protect from None
                continue
            elif isinstance(p, FDSParam):  # FDSParam
                ps.append(p)
                msgs.append(p.msg)
            elif isinstance(p, tuple):
                if isinstance(p[0], FDSParam):  # many: FDSParam, FDSParam
                    ps.extend(p)
                    msgs.extend(pi.msg for pi in p)
                elif isinstance(p[0], tuple):  # multi: (FDSParam, FDSParam), ...
                    mps = p
                    msgs.extend(pi.msg for pi in mps[0])  # only from first
            else:
                raise ValueError(f"Unrecognized item <{p}>")
        # Assemble parameters of namelist nls
        nls = list()
        ps = list(str(p) for p in ps)  # invariant param strings
        if mps:
            if ps and ps[0][0:3] == "ID=":
                ps.pop(0)  # remove ID, if available and first
            for multip in mps:
                nl = list(str(p) for p in multip if p)  # variant param strings
                nl.extend(ps)  # invariant
                nl.append("/")  # end
                nls.append(nl)
        else:
            nl = ps  # invariant
            nl.append("/")  # end
            nls.append(nl)
        # Create result string
        max_len = self.max_len - 5  # because len("&OBST") == 5
        lines = list(f"! {m}" for m in msgs if m)  # all messages
        for nl in nls:
            nl.reverse()  # for efficient pop
            line = f"&{self.label}"
            current_len = 5  # because len("&OBST") == 5
            while nl:
                p = nl.pop()
                l = 1 + len(p)  # l = sep + chunk
                if current_len == 5 or current_len + l <= max_len:
                    line = " ".join((line, p))
                    current_len += l
                else:  # split long line
                    line = "\n      ".join((line, p))
                    current_len = 5 + l
            lines.append(line)
        return "\n".join(lines)

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
    )  # no MULTILINE, so that $ is the end of the file

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
    """Python datastructure for FDS case.

    fds_namelists: list of FDSNamelist instances
    """

    def __init__(self, fds_namelists=None):
        self.fds_namelists = fds_namelists or list()

    def __str__(self):
        return "\n".join(str(n) for n in self.fds_namelists)

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
