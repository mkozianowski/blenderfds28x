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
    Exception raised by BlenderFDS methods in case of an error.
    """

    def __init__(self, sender, msg):
        """!
        Class constructor.
        @param sender: the object that generates the exception
        @param msg: exception message
        """
        ## The object that generates the exception
        self.sender = sender
        ## Exception message
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
    """!
    Blender representation of an FDS parameter.
    """

    ## Object label
    label = "No Label"
    ## Object description
    description = None
    ## Unique integer id for EnumProperty
    enum_id = None
    ## Other BlenderFDS parameters, eg: {'draw_type': 'WIRE', ...}
    bf_other = {}
    ## My sub params, tuple of classes of type BFParam
    bf_params = tuple()
    ## FDS label, eg. "OBST", "ID", ...
    fds_label = None
    ## FDS default value
    fds_default = None
    ## type in bpy.types for Blender property, eg. Object
    bpy_type = None
    ## idname of related bpy.types Blender property, eg. "bf_id"
    bpy_idname = None
    ## prop in bpy.props of Blender property, eg. StringProperty
    bpy_prop = None
    ## Blender property default
    bpy_default = None
    ## Other optional Blender property parameters, eg. {"min": 3., ...}
    bpy_other = {}
    ## idname of export toggle Blender property
    bpy_export = None
    ## idname of export toggle Blender property
    bpy_export_default = None

    ## List of registered bpy_idname for unregistering
    _registered_bpy_idnames = list()

    def __init__(self, element):
        """!
        Class constructor.
        @param element: FDS element represented by this class instance.
        """
        ## FDS element represented by this class instance
        self.element = element

    def __str__(self):
        return f"{self.label}(element='{self.element.name}')"

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
            cls.description += f"\n(FDS default: {cls.fds_default})"
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
            cls._registered_bpy_idnames.append(cls.bpy_idname)
        # Create bpy_export
        if cls.bpy_export:
            if hasattr(cls.bpy_type, cls.bpy_export):
                log.debug(f"Using <{cls.bpy_export}> Blender property as export ref")
                if cls.bpy_export_default is not None:
                    raise Exception(
                        f"Unused bpy_export_default in class <{cls.__name__}>"
                    )
            else:
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
                cls._registered_bpy_idnames.append(cls.bpy_export)

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be registered.
        """
        for bpy_idname in cls._registered_bpy_idnames:
            if not hasattr(cls.bpy_type, bpy_idname):  # already deleted?
                continue
            log.debug(f"Unregistering <{bpy_idname}> Blender property")
            delattr(cls.bpy_type, bpy_idname)

    @property
    def value(self):
        """!
        Return value from element instance.
        @return any type
        """
        return getattr(self.element, self.bpy_idname)

    def set_value(self, context, value=None):
        """!
        Set element instance value. If value is None, set default value.
        @param context: the Blender context.
        @param value: value to set.
        """
        if value is None:
            setattr(self.element, self.bpy_idname, self.bpy_default)
            return
        else:
            setattr(self.element, self.bpy_idname, value)
            return

    @property
    def exported(self):
        """!
        Return True if self is exported to FDS.
        """
        # Check if empty
        value = self.value
        if value is None or value == "":
            return False
        # Check if identical to FDS default
        d = self.fds_default
        if d is not None:
            if isinstance(value, float):  # floats comparison
                return value > d + 1e-6 or value < d - 1e-6
            elif value == d:  # other comparison
                return False
        # Check if bpy_export is True
        if self.bpy_export is None:
            return True
        return bool(getattr(self.element, self.bpy_export, True))

    def set_exported(self, context, value=None):
        """!
        Set if self is exported to FDS. If value is None, set default value.
        @param context: the Blender context.
        @param value: value to set.
        """
        if self.bpy_export is None:
            if not value:
                raise Exception(f"Cannot set self.exported = False in <{self}>")
        else:
            if value is None:
                setattr(self.element, self.bpy_export, self.bpy_export_default)
            else:
                setattr(self.element, self.bpy_export, value)

    def check(self, context):
        """!
        Check self validity for FDS, in case of error raise BFException.
        @param context: the Blender context.
        """
        pass

    def draw_operators(self, context, layout):
        """!
        Draw my operators on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        pass

    def draw(self, context, layout):
        """!
        Draw my UI on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
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
        return col

    def to_fds_param(self, context):
        """!
        Return the FDSParam representation of element instance.
        @param context: the Blender context.
        @return None, FDSParam, (FDSParam, ...) called "many", or ((FDSParam, ...), ...) called "multi" instances.
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
        return FDSParam(
            fds_label=self.fds_label,
            values=values,
            precision=self.bpy_other.get("precision", 3),
        )

    # TODO to_fds

    def from_fds(self, context, value):  # TODO change sig
        """!
        Set self.value from py value, on error raise BFException.
        @param context: the Blender context.
        @param value: the value to set.
        """
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
    Blender representation of an FDS parameter, helper for FDS XB parameter.
    """

    pass


class BFParamXYZ(BFParam):
    """!
    Blender representation of an FDS parameter, helper for FDS XYZ parameter.
    """

    pass


class BFParamPB(BFParam):
    """!
    Blender representation of an FDS parameter, helper for FDS PBX PBY PBZ parameters.
    """

    pass


class BFParamStr(BFParam):
    """!
    Blender representation of an FDS parameter, helper for any FDS string parameter.
    """

    bpy_prop = StringProperty

    # def check(self, context):  # No check
    #     value = self.value
    #     if "&" in value or "/" in value or "#" in value:
    #         raise BFException(self, "<&>, </>, and <#> characters not allowed")
    #     # if (
    #     #     "'" in value
    #     #     or '"' in value
    #     #     or "`" in value
    #     #     or "“" in value
    #     #     or "”" in value
    #     #     or "‘" in value
    #     #     or "’‌" in value
    #     # ):
    #     #     raise BFException(self, "Quote characters not allowed")


class BFParamFYI(BFParamStr):
    """!
    Blender representation of an FDS parameter, helper for FDS FYI parameter.
    """

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
    """!
    Blender representation of any FDS parameter, helper for the 'other' FDS parameters.
    """

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_type = Object  # example
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

    @property
    def value(self):
        collection = getattr(self.element, self.bpy_idname)
        return tuple(item.name for item in collection if item.bf_export)

    def set_value(self, context, value):
        collection = getattr(self.element, self.bpy_idname)
        if value is None:
            collection.clear()
        else:
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
                (FDSParam(fds_label=p.name) for p in coll if p.bf_export and p.name)
            )  # many


# BFNamelist is a special BFParam


class BFNamelist(BFParam):
    """!
    Blender representation of an FDS namelist group.
    """

    def __init__(self, element):
        ## FDS element represented by this class instance
        self.element = element
        ## My sub params, tuple of element instances of type BFParam
        self.bf_params = tuple(p(element) for p in self.bf_params)

    # inherits __str__

    @classmethod
    def register(cls):
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

    def get_bf_param_by_fds_label(self, fds_label):
        """!
        Return bf_param (class or instance) by its fds_label.
        @param fds_label: FDS parameter to be obtained.
        @return BFParam or None.
        """
        i = self._bf_param_idx_by_fds_label.get(fds_label)
        if i is not None:
            return self.bf_params[i]

    @property
    def bf_param_xb(self):
        """!
        Return the reference of the XB bf_param (BFParamXB class or instance).
        """
        if self._bf_param_xb_idx is not None:
            return self.bf_params[self._bf_param_xb_idx]

    @property
    def bf_param_xyz(self):
        """!
        Return the reference of the XYZ bf_param (BFParamXYZ class or instance).
        """
        if self._bf_param_xyz_idx is not None:
            return self.bf_params[self._bf_param_xyz_idx]

    @property
    def bf_param_pb(self):
        """!
        Return the reference of the PB bf_param (BFParamPB class or instance).
        """
        if self._bf_param_pb_idx is not None:
            return self.bf_params[self._bf_param_pb_idx]

    @property
    def bf_param_other(self):
        """!
        Return the reference of the other bf_param (class or instance).
        """
        if self._bf_param_other_idx is not None:
            return self.bf_params[self._bf_param_other_idx]

    @property
    def exported(self):
        if self.bpy_export is None:
            return True
        return bool(getattr(self.element, self.bpy_export, True))

    def draw_operators(self, context, layout):
        """!
        Draw my operators on Tools panel.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        layout.label(text="No tool available.")

    def draw(self, context, layout):
        """!
        Draw my UI on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
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

    def to_fds_namelist(self, context):
        """!
        Return the FDSNamelist representation of element instance.
        @param context: the Blender context.
        @return None, FDSNamelist, or (FDSNamelist, ...) instances.
        """
        # Get if exported and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # Assemble from bf_params, protect from None
        return FDSNamelist(
            fds_label=self.fds_label,
            fds_params=list(
                p.to_fds_param(context) for p in self.bf_params if p is not None
            ),
        )

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return None or FDS formatted string, eg. "&OBST ID='Test' /".
        """
        result = self.to_fds_namelist(context)
        if result is None:  # protect from None
            return
        if is_iterable(result):  # many (FDSNamelist, ...), multi not allowed!
            return "\n".join((r.to_fds() for r in result if r is not None))
        else:  # single FDSNameslist
            return result.to_fds()

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_params values from list of FDSParam, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: instance of type FDSNamelist.
        """
        for p in fds_namelist.fds_params:
            bf_param = self.get_bf_param_by_fds_label(p.fds_label)
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
    """!
    Blender representation of an FDS namelist group related to a Blender Scene.
    """

    bpy_type = Scene


class BFNamelistOb(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Object.
    """

    bpy_type = Object

    @property
    def exported(self):
        return not self.element.hide_render

    def set_exported(self, context, value=None):
        if value is None:
            self.element.hide_render = not self.bpy_export_default
        else:
            self.element.hide_render = not bool(value)


class BFNamelistMa(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Material.
    """

    bpy_type = Material


# Python representations of FDS entities


class FDSParam:
    """!
    Python datastructure representing an FDS namelist parameter.
    """

    def __init__(
        self, fds_label, values=None, precision=3, exponential=False, msg=None
    ):
        """!
        Class constructor.
        @param label: namelist parameter label.
        @param values: list of parameter values of type float, int, str, bool.
        @param precision: float precision, number of decimal digits.
        @param exponential: if True sets exponential representation of floats.
        @param msg: comment message.
        """
        ## namelist parameter label
        self.fds_label = fds_label
        ## list of parameter values of type float, int, str, bool.
        self.values = values or list()
        ## float precision, number of decimal digits.
        self.precision = precision
        ## if True sets exponential representation of floats.
        self.exponential = exponential
        ## comment message.
        self.msg = msg

    def __str__(self):
        result = self.to_fds()
        if len(result) > 80:
            return result[:37] + " ... " + result[-37:]
        return result

    @property
    def formatted_values(self):
        """!
        Return the list of FDS formatted values, eg. "'Test1'","'Test2'"
        """
        try:
            v0 = self.values[0]
        except IndexError:
            return list()
        strings = list()
        if isinstance(v0, float):
            p = self.precision
            if self.exponential:
                strings.extend(f"{round(v,p):.{p}E}" for v in self.values)
            else:
                strings.extend(f"{round(v,p):.{p}f}" for v in self.values)
        elif isinstance(v0, str):
            strings.extend("'" in v and f'"{v}"' or f"'{v}'" for v in self.values)
        elif isinstance(v0, bool):  # always before int
            strings.extend(v and "T" or "F" for v in self.values)
        elif isinstance(v0, int):
            strings.extend(str(v) for v in self.values)
        else:
            raise ValueError(f"Unknown value type for parameter <{self.fds_label}>")
        return strings

    _re_decimal = r"\.([0-9]+)"  # decimal positions

    _scan_decimal = re.compile(_re_decimal, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _re_integer = r"([0-9]*)\.?[0-9]*[eE]"  # integer postions of exp notation

    _scan_integer = re.compile(_re_integer, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    def to_fds(self):
        """!
        Return the FDS formatted string.
        @return FDS formatted string, eg. "ID='Test'"
        """
        v = ",".join(self.formatted_values)
        if not v:
            return self.fds_label
        return self.fds_label + "=" + v

    def from_fds(self, f90_values):
        """!
        Import from FDS formatted string of values, on error raise BFException.
        @param f90_values: FDS formatted string of values, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        f90_values += ","  # always return a tuple, eg. "34" -> "34,"
        # Translate F90 booleans
        if f90_values[0] == ".":
            f90_values = (
                f90_values.upper().replace(".TRUE.", "True").replace(".FALSE.", "False")
            )
        elif f90_values[0] in ("T", "F"):
            f90_values = f90_values.upper().replace("T", "True").replace("F", "False")
        # Python evaluation of F90 value, after cleaning from \n \r
        f90_values = " ".join(f90_values.split())
        try:
            self.values = eval(f90_values)
        except Exception as err:
            raise BFException(
                self,
                f"Parsing error in parameter <{self.fds_label}={f90_values} ... />\n{err}",
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
    Python datastructure representing an FDS namelist.
    """

    ## max number of columns of formatted output
    maxlen = 80  # TODO to config

    def __init__(self, fds_label, fds_params=None, msg=None):
        """!
        Class constructor.
        @param fds_label: namelist group label.
        @param fds_params: list of FDSParam instances.
                Can contain one and only one list of lists of FDSParam instances
                to represent multiple params: (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...).
        @param msg: comment message.
        """
        ## namelist group label
        self.fds_label = fds_label
        ## list of FDSParam instances
        self.fds_params = fds_params or list()
        ## comment message
        self.msg = msg

    def __str__(self):
        return self.to_fds()

    def get_fds_param_by_label(self, fds_label) -> "FDSParam or None":
        """!
        Return the fds_param by its label.
        @param fds_label: namelist parameter label.
        @return None or FDSParam.
        """
        for p in self.fds_params:
            if p.fds_label == fds_label:
                return p

    def to_fds(self):
        """!
        Return the FDS formatted string.
        @return FDS formatted string, eg. "&OBST ID='Test' /".
        """
        # Mix parameters
        invps = list()  # invariant parameters
        multips = list()  # multi parameters
        msgs = list((self.msg,))  # messages
        for p in self.fds_params:
            # Empty
            if p is None:  # Protect from None
                continue
            # Invariant
            elif isinstance(p, FDSParam):  # single
                invps.append(p)
                msgs.append(p.msg)
            elif isinstance(p, tuple):  # many or multi
                if isinstance(p[0], FDSParam):  # many
                    invps.extend(p)
                    msgs.extend(pi.msg for pi in p)
                elif isinstance(p[0], tuple):  # multi
                    multips = p
                    msgs.extend(
                        p0.msg for p0 in multips[0]
                    )  # msg only from first many of multi
            else:
                raise ValueError(f"Unrecognized type of <{p}>")
        # Treat invariant, many and multi parameters
        # nl = FDSParam, FDSParam, ...
        nls = list()  # list of nl
        if multips:
            # Remove ID parameter, as multi embeds a new indexed ID.
            for i, p in enumerate(invps):
                if p.fds_label == "ID":
                    invps.pop(i)
                    break
            # Add nl with one of multips + invps
            for multip in multips:
                nl = list(multip)
                nl.extend(invps)
                nls.append(nl)
        else:
            nls.append(invps)
        # Prepare strings
        lines = list(f"! {m}" for m in msgs if m)  # all messages
        for nl in nls:
            newline = False
            line = "&" + self.fds_label
            for p in nl:
                label = p.fds_label
                vs = p.formatted_values  # list of str
                if not vs:  # no values
                    if not newline and len(line) + 1 + len(label) <= self.maxlen:
                        # Parameter to the same line
                        newline = False
                        line += " " + label
                    else:
                        # Parameter to new line
                        lines.append(line)
                        line = "      " + label  # new line
                else:  # values
                    v = ",".join(vs)  # values str
                    if (
                        not newline
                        and len(line) + 1 + len(label) + 1 + len(v) <= self.maxlen
                    ):
                        # Parameter to the same line
                        newline = False
                        line += " " + label + "=" + v
                    else:
                        # Parameter to new line
                        lines.append(line)
                        line = "      " + label + "="  # new line
                        if len(line) + len(v) <= self.maxlen:
                            # Values do not need splitting
                            line += v
                        else:
                            # Values need splitting
                            newline = True  # the following needs a new line
                            for v in vs:
                                if len(line) + len(v) + 1 <= self.maxlen:
                                    line += v + ","
                                else:
                                    lines.append(line)
                                    line = "        " + v + ","  # new line
                            line = line[:-1]  # remove last ","
            line += " /"
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

    def from_fds(self, f90_params):  # TODO change signature: f90_namelist
        """!
        Import from FDS formatted string of parameters, on error raise BFException.
        @param f90_params: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        for match in re.finditer(self._scan, f90_params):
            label, f90_values, following_label = match.groups()
            p = FDSParam(fds_label=label)
            p.from_fds(f90_values=f90_values)
            self.fds_params.append(p)
            if following_label is None:
                break


class FDSCase:
    """!
    Python datastructure representing an FDS case.
    """

    def __init__(self, fds_namelists=None):
        """!
        Class constructor.
        @param fds_namelists: list of FDSNamelist instances.
        """
        self.fds_namelists = fds_namelists or list()

    def __str__(self):
        return self.to_fds()

    def get_fds_namelists_by_label(self, label):
        """!
        Return the tuple of fds_namelists by their label.
        @param label: namelist parameter label.
        @return tuple of fds namelists.
        """
        return tuple(n for n in self.fds_namelists if n.fds_label == label)

    def to_fds(self):
        """!
        Return the FDS formatted string.
        @return FDS formatted string, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        return "\n".join(
            n.to_fds() for n in self.fds_namelists if n is not None
        )  # Protect None

    _scan = re.compile(
        r"""
        (?:^&)             # & at the beginning
        ([A-Z]+[A-Z0-9]*)  # namelist label
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE | re.MULTILINE,
    )  # MULTILINE, so that ^ is the beginning of each line

    def from_fds(self, f90_namelists, reset=True):
        """!
        Import from FDS formatted string of namelists, on error raise BFException.
        @param f90_namelists: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        @param reset: if True, reset self.fds_namelists to empty list before importing.
        """
        if reset:
            self.fds_namelists = list()
        for match in re.finditer(self._scan, f90_namelists):
            nl = FDSNamelist(fds_label=match.groups()[0])
            nl.from_fds(f90_params=f90_namelists[match.end() :])
            self.fds_namelists.append(nl)
