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

if __name__ != "__main__":
    from .bl import custom_uilist
    from .utils import is_iterable

log = logging.getLogger(__name__)

# BF specific exception


class BFException(Exception):
    """!
    Exception raised by methods in case of a BF error.
    """

    def __init__(self, sender, msg):
        """!
        Constructor of the Exception
        @param sender: the object that generate the exception
        @param msg: exception message
        """
        self.sender = sender
        self.msg = msg

    def __str__(self):
        """!
        String representation
        """
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
    """!
    Blender representation of an FDS parameter.
    """

    label = "No Label"  # Object label
    description = None  # Object description
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
        """!
        Class constructor.
        @param element: FDS element to represent.
        """
        self.element = element

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
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
        # Create bpy_prop
        if cls.bpy_prop:
            if not cls.bpy_idname or not cls.label or not cls.description:
                raise Exception(f"No bpy_idname, label or description in class <{cls}>")
            bpy_other = cls.bpy_other.copy()
            if cls.bpy_default is not None:
                bpy_other["default"] = cls.bpy_default
            log.debug(f"Setting <{cls.bpy_idname}> Blender property")
            setattr(
                cls.bpy_type,
                cls.bpy_idname,
                cls.bpy_prop(name=cls.label, description=cls.description, **bpy_other),
            )
        # Create bpy_export
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
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        # if cls.bpy_prop and cls.bpy_idname:
        #     delattr(cls.bpy_type, cls.bpy_idname)
        # if cls.bpy_export:
        #     delattr(cls.bpy_type, cls.bpy_export)
        pass

    @property
    def value(self) -> "any":
        """!
        Get value.
        @return value to be get
        """
        return getattr(self.element, self.bpy_idname)

    def set_value(self, context, value=None):
        """!
        Set value. If value is None, set defaul.
        @param context: data in the current active view.
        @param value: value to set.
        """
        if value is None:
            setattr(self.element, self.bpy_idname, self.bpy_default)
            return
        else:
            setattr(self.element, self.bpy_idname, value)
            return

    @property
    def exported(self) -> "bool":
        """!
        Get if self is exported.
        @return True if is exported, False otherwise.
        """
        # Check if empty
        value = self.value
        if value is None or value == "":
            return False
        # Check if FDS default
        d = self.fds_default
        if d is not None:
            if isinstance(value, float):  # floats comparison
                return value > d + 1e-6 or value < d - 1e-6
            elif value == d:  # other comparison
                return False
        # Check if bpy_export
        return bool(getattr(self.element, str(self.bpy_export), True))

    def set_exported(self, context, value=None):
        """!
        Set if self is exported. If value is None, set defaul
        @param context: data in the current active view.
        @param value: value to set.
        """
        if self.bpy_export:
            if value is None:
                setattr(self.element, self.bpy_export, self.bpy_export_default)
            else:
                setattr(self.element, self.bpy_export, value)

    def check(self, context):
        """
        !Check self validity.
        @param context: data in the current active view.
        """
        pass

    def draw_operators(self, context, layout):
        """!Draw operators on layout.
        @param context: data in the current active view.
        @param layout: ???
        """
        pass

    def draw(self, context, layout) -> "layout":
        """!
        Draw self UI on layout.
        @param context: data in the current active view.
        @param layout: ???
        @return layout column.
        """
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
        row = col.row(align=True)
        row.prop(self.element, self.bpy_idname, text=self.label)
        self.draw_operators(context, row)
        if self.bpy_export:
            row.prop(self.element, self.bpy_export, text="")
        # else:
        #     col.prop(self.element, self.bpy_idname, text=self.label)
        #     self.draw_operators(context, row)
        return col

    def to_fds_param(self, context) -> "None, FDSParam, or ((FDSParam, ...), ...)":
        """!
        Return the FDSParam Python representation.
        @param context: data in the current active view.
        @return FDSParam representation of the context.
        """
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
        """!
        Return the FDS str representation.
        @param context: data in the current active view.
        @return string representation.
        """
        fds_param = self.to_fds_param(context)
        return fds_param and str(fds_param)

    def from_fds(self, context, value):
        """!
        Set parameter value from value in FDS notation, on error raise BFException.
        @param context: data in the current active view.
        @param value: the value to set.
        """
        log.debug(f"{self} {value}")
        if (
            value is not None and len(value) == 1
        ):  # FDSParam.values is always a list, value can be None
            value = value[0]
        try:
            self.set_value(context, value)
        except Exception as err:
            raise BFException(self, f"Error importing <{value}> value, {str(err)}")
        self.set_exported(context, True)


class BFParamXB(BFParam):
    """!
    Helper for FDS XB parameter.
    """

    pass


class BFParamXYZ(BFParam):
    """!
    Helper for FDS XYZ parameter.
    """

    pass


class BFParamPB(BFParam):
    """!
    Helper for FDS PB parameter.
    """

    pass


class BFParamStr(BFParam):
    """!
    Helper for FDS string parameter.
    """

    bpy_prop = StringProperty

    def check(self, context):
        """!
        Checking the value validity.
        @param context: data in the current active view.
        """
        value = self.value
        if "&" in value or "/" in value or "#" in value:
            raise BFException(self, "<&>, </>, and <#> characters not allowed")
        # if (
        #     "'" in value
        #     or '"' in value
        #     or "`" in value
        #     or "“" in value
        #     or "”" in value
        #     or "‘" in value
        #     or "’‌" in value
        # ):
        #     raise BFException(self, "Quote characters not allowed")


class BFParamFYI(BFParamStr):
    """!
    Helper for FDS FYI parameter.
    """

    label = "FYI"
    description = "For your information"
    fds_label = "FYI"
    bpy_idname = "bf_fyi"
    bpy_prop = StringProperty
    bpy_other = {"maxlen": 128}

    def draw(self, context, layout):
        """!
        ???
        @param context: data in the current active view.
        @param layout: ???
        """
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        if self.bpy_idname:
            col.prop(self.element, self.bpy_idname, text="", icon="INFO")
        return col


class BFParamOther(BFParam):
    """!
    Helper for FDS other parameters.
    """

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_type = Object  # example
    bpy_idname = "bf_other"

    bpy_pg = None  # PropertyGroup, eg. WM_PG_bf_other
    bpy_ul = None  # UIList, eg. WM_UL_bf_other_items

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        cls._ops = custom_uilist.register_collection(
            bpy_type=cls.bpy_type,
            bpy_idname=cls.bpy_idname,
            name=cls.label,
            bpy_pg=cls.bpy_pg,
            description=cls.description,
        )

    @classmethod
    def unregister(cls):
        """!
        Register related Blender properties.
        @param cls: class to be unregistered.
        """
        custom_uilist.unregister_collection(
            bpy_type=cls.bpy_type, bpy_idname=cls.bpy_idname, ops=cls._ops
        )

    @property
    def value(self):
        """!
        Get value.
        @return value to be get
        """
        collection = getattr(self.element, self.bpy_idname)
        return tuple(item.name for item in collection if item.bf_export)

    def set_value(self, context, value):
        """!
        Set value.
        @param context: data in the current active view.
        @param value: value to set.
        """
        collection = getattr(self.element, self.bpy_idname)
        if value is None:
            collection.clear()
        else:
            item = collection.add()
            item.name, item.bf_export = value, True

    def draw(self, context, layout):
        """!
        Draw self UI on layout.
        @param context: data in the current active view.
        @param layout: ???
        """
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
        """!
        Return the FDSParam Python representation.
        @param context: data in the current active view.
        @return FDSParam representation of the context.
        """
        self.check(context)
        coll = getattr(self.element, self.bpy_idname)
        if coll:
            return tuple(
                (FDSParam(label=p.name) for p in coll if p.bf_export and p.name)
            )  # many


# BFNamelist is a special BFParam


class BFNamelist(BFParam):
    """!
    Blender representation of an FDS namelist group.
    """

    maxlen = 80  # max columns when exporting

    def __init__(self, element):
        """!
        Class constructor
        @param element: FDS element to represent.
        """
        self.element = element
        self.bf_params = tuple(p(element) for p in self.bf_params)  # instantiate

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        super().register()
        # Indexes are used to link both the class and the instance
        cls._bf_param_idx_by_fds_label = dict()  # fds_label: index of param
        cls._bf_param_xb_idx = None  # index of param of type BFParamXB
        cls._bf_param_xyz_idx = None  # ... of type BFParamXYZ
        cls._bf_param_pb_idx = None  # ... of type BFParamPB
        cls._bf_param_other_idx = None  # ... of type BFParamOther
        for i, p in enumerate(cls.bf_params):
            if p.fds_label:
                cls._bf_param_idx_by_fds_label[p.fds_label] = i
            if issubclass(p, BFParamXB):
                cls._bf_param_xb_idx = i
            elif issubclass(p, BFParamXYZ):
                cls._bf_param_xyz_idx = i
            elif issubclass(p, BFParamPB):
                cls._bf_param_pb_idx = i
            elif issubclass(p, BFParamOther):
                cls._bf_param_other_idx = i

    def get_bf_param_by_fds_label(self, fds_label) -> "BFParam or None":
        """!
        Get bf_param (class or instance) by its fds_label.
        @param fds_label: FDS parameter to be obtained.
        @return FDS parameter.
        """
        i = self._bf_param_idx_by_fds_label.get(fds_label)
        if i is not None:
            return self.bf_params[i]

    @property
    def bf_param_xb(self):
        """!
        XB bf_param (class or instance).
        @return BFParamXB parameter.
        """
        if self._bf_param_xb_idx is not None:
            return self.bf_params[self._bf_param_xb_idx]

    @property
    def bf_param_xyz(self):
        """!
        XYZ bf_param (class or instance).
        @return BFParamXYZ parameter.
        """
        if self._bf_param_xyz_idx is not None:
            return self.bf_params[self._bf_param_xyz_idx]

    @property
    def bf_param_pb(self):
        """!
        PB bf_param (class or instance).
        @return BFParamPB parameter.
        """
        if self._bf_param_pb_idx is not None:
            return self.bf_params[self._bf_param_pb_idx]

    @property
    def bf_param_other(self):
        """!
        Other bf_param (class or instance).
        @return BFParamOther parameter.
        """
        if self._bf_param_other_idx is not None:
            return self.bf_params[self._bf_param_other_idx]

    def draw(self, context, layout) -> "layout":
        """!
        Draw self UI on layout.
        @param context: data in the current active view.
        @param layout: ???
        @return layout column.
        """
        # Check and active
        try:
            self.check(context)
        except BFException:
            layout.alert = True
        layout.active = self.exported
        # Parameters
        col = layout.column()
        for p in self.bf_params:
            p.draw(context, col)
        return col

    @property
    def exported(self) -> "bool":
        """!
        Get if self is exported.
        @return True if is exported, False otherwise.
        """
        if self.bpy_export is None:
            return True
        return bool(getattr(self.element, self.bpy_export, True))

    def to_fds_namelist(self, context) -> "None or FDSNamelist":
        """!
        Return the FDSNamelist Python representation.
        @param context: data in the current active view.
        @return FDSNamelist.
        """
        # Get if exported and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # Assemble from bf_params
        return FDSNamelist(
            label=self.fds_label,
            fds_params=list(p.to_fds_param(context) for p in self.bf_params if p),
            maxlen=self.maxlen,
        )

    def to_fds(self, context) -> "None or str":
        """!
        Return the FDS str representation.
        @param context: data in the current active view.
        @return string representation.
        """
        fds_namelist = self.to_fds_namelist(context)
        return fds_namelist and str(fds_namelist)

    def from_fds(self, context, fds_params):
        """!
        Set namelist parameter values from list of FDSParam, on error raise BFException.
        @param context: data in the current active view.
        @param fds_params: fds parameter to set.
        """
        for p in fds_params:
            bf_param = self.get_bf_param_by_fds_label(p.label)
            if bf_param is None:
                bf_param_other = self.bf_param_other
                if bf_param_other:
                    bf_param_other.set_value(context, value=str(p))
                else:
                    raise BFException(self, f"Value {p} is not managed")
                continue
            else:
                bf_param.from_fds(context, p.values)
        self.set_exported(context, True)


class BFNamelistSc(BFNamelist):
    bpy_type = Scene


class BFNamelistOb(BFNamelist):
    bpy_type = Object

    @property
    def exported(self) -> "bool":
        """Get if self is exported."""
        return not self.element.hide_render

    def set_exported(self, context, value=None):
        """Set if self is exported."""
        if value is None:
            self.element.hide_render = not self.bpy_export_default
        else:
            self.element.hide_render = not bool(value)


class BFNamelistMa(BFNamelist):
    bpy_type = Material


# Python representations of FDS entities


class FDSParam:
    """!
    Python datastructure for FDS namelist parameter.
    """

    def __init__(self, label, values=None, precision=3, exponential=False, msg=None):
        """!
        Class constructor.
        @param label: namelist parameter label.
        @param values: list of parameter values of type float, int, str, bool.
        @param precision: float precision, number of decimal digits.
        @param exponential: ???
        @param msg: comment msg.
        """
        self.label = label
        self.values = values or list()
        self.precision = precision
        self.exponential = exponential
        self.msg = msg

    def __str__(self):
        """!
        String representation
        """
        if not self.values:
            return self.label
        # Check first element of the iterable and choose formatting
        v0 = self.values[0]
        if isinstance(v0, float):
            if self.exponential:
                v_string = ",".join(f"{v:.{self.precision}E}" for v in self.values)
            else:
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

    _re_decimal = r"\.([0-9]+)"  # decimal positions

    _scan_decimal = re.compile(_re_decimal, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _re_integer = r"([0-9]*)\.?[0-9]*[eE]"  # integer postions of exp notation

    _scan_integer = re.compile(_re_integer, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    def from_fds(self, f90_values):
        """!
        Import from f90 values.
        @param f90_values: f90 string value (eg. "2.34, 1.23, 3.44").
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
        except Exception as err:
            raise BFException(
                self,
                f"Parsing error in parameter <{self.label}={f90_values} ... />\n{err}",
            )
        # Get precision from the first f90 float value
        if isinstance(self.values[0], float):
            match = re.findall(self._re_decimal, f90_values)
            if match:
                self.precision = max(len(m) for m in match)
            else:
                self.precision = 1
            # Exp notation?
            match = re.findall(self._re_integer, f90_values)
            if match:
                self.exponential = True
                self.precision += max(len(m) for m in match) - 1


class FDSNamelist:
    """!
    Python datastructure for FDS namelist.
    """

    def __init__(self, label, fds_params=None, msg=None, maxlen=80):
        """!
        Class constructor.
        @param label: namelist group label, string
        @param fds_params: list of FDSParam instances.
                Can contain one and only one list of lists of FDSParam instances
                to represent multiple params: (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...)
        @param msg: comment msg
        @param maxlen: max columns of formatted output
        """
        self.label = label
        self.fds_params = fds_params or list()
        self.msg = msg
        self.maxlen = maxlen

    def __str__(self):
        """!
        String representation
        """
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
                nls.append(nl)
        else:
            nl = ps  # invariant
            nls.append(nl)
        # Create result string
        maxlen = self.maxlen - 5  # because len("&OBST") == 5
        lines = list(f"! {m}" for m in msgs if m)  # all messages
        for nl in nls:
            nl.reverse()  # for efficient pop
            line = f"&{self.label}"
            current_len = 5  # because len("&OBST") == 5
            while nl:
                p = nl.pop()
                l = 1 + len(p)  # l = sep + chunk
                if current_len == 5 or current_len + l <= maxlen:
                    line = " ".join((line, p))
                    current_len += l
                else:  # split long line
                    line = "\n      ".join((line, p))
                    current_len = 5 + l
            line = "".join((line, " /"))  # close
            lines.append(line)
        return "\n".join(lines)

    _re_label = r"([A-Z][A-Z0-9_\(\):,]*?)"  # param label w indexes
    _re_space = r"[\s\t]*"  # zero or more spaces
    _re_sep = r"[,\s\t]+"  # one or more separators
    _re_end = r"[,\s\t]*/"  # zero or more separators + "/"
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
            p.from_fds(f90_values=f90_values)
            self.fds_params.append(p)
            if following_label is None:
                break

    def get_fds_param_by_label(self, label) -> "FDSParam or None":
        """Get fds_param by its label."""
        for p in self.fds_params:
            if p.label == label:
                return p


class FDSCase:
    """!
    Python datastructure for FDS case.
    """

    def __init__(self, fds_namelists=None):
        """!
        Class constructor.
        @param fds_namelists: list of FDSNamelist instances
        """
        self.fds_namelists = fds_namelists or list()

    def __str__(self):
        """!
        String representation
        """
        return "\n".join(str(n) for n in self.fds_namelists)

    _scan = re.compile(
        r"""
        (?:^&)             # & at the beginning
        ([A-Z]+[A-Z0-9]*)  # namelist label
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE | re.MULTILINE,
    )  # MULTILINE, so that ^ is the beginning of each line

    def from_fds(self, f90_namelists, reset=True):
        """!
        Import from f90 namelists.
        @param f90_namelists: f90 namelists string (eg. "&OBST ... /\n&DEVC ... /").
        @param reset: ???
        """
        if reset:
            self.fds_namelists = list()
        for match in re.finditer(self._scan, f90_namelists):
            nl = FDSNamelist(label=match.groups()[0])
            nl.from_fds(f90_params=f90_namelists[match.end() :])
            self.fds_namelists.append(nl)

    def get_fds_namelists_by_label(self, label) -> "tuple of FDSNamelist":
        """!
        Get tuple of fds_namelists by label.
        @param label: label of the f90 namelists.
        @return tuple of fds namelists
        """
        return tuple(n for n in self.fds_namelists if n.label == label)


if __name__ == "__main__":
    import sys

    if not sys.argv:
        exit()
    with open(sys.argv[1], "r") as f:
        fdsfile = f.read()
    c = FDSCase()
    c.from_fds(fdsfile)
    print(c)
