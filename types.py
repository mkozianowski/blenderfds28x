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

import re, os.path

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

from .config import separator, comment
from .bl.custom_uilist import get_ops, draw_ops
from .utils import is_iterable


class BFException(Exception):
    """Exception raised by all methods in case of an error."""

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
        return comment(str(self))


class Parameter:
    """Generic FDS parameter."""

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
        print(f"BFDS: registering <{cls.label}>")
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
                print(
                    f"BFDS:   Warning: Cannot redefine bpy_idname <{cls.bpy_idname}> in class <{cls}>"
                )
                return
            bpy_other = cls.bpy_other.copy()
            if cls.bpy_default is not None:
                bpy_other["default"] = cls.bpy_default
            print(f"BFDS:   setting <{cls.bpy_idname}> Blender property")
            setattr(
                cls.bpy_type,
                cls.bpy_idname,
                cls.bpy_prop(name=cls.label, description=cls.description, **bpy_other),
            )
        # Check and create bpy_export
        if cls.bpy_export and not hasattr(cls.bpy_type, cls.bpy_export):
            print(f"BFDS:   setting <{cls.bpy_export}> Blender property")
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
        """Get parameter value."""
        if not self.bpy_idname:
            raise Exception(f"value() not implemented in class <{self.__class__}>")
        return getattr(self.element, self.bpy_idname)

    # set_value # FIXME

    @property
    def exported(self) -> "bool":
        """Get if parameter is exported."""
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

    def check(self, context):
        """Check parameter validity."""
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
        # Check first element of the iterable and choose formatting
        if isinstance(values[0], bool):
            value = ",".join(v and "T" or "F" for v in values)
        elif isinstance(values[0], int):
            value = ",".join(str(v) for v in values)
        elif isinstance(values[0], float):
            p = self.bpy_other.get("precision", 3)
            value = ",".join(f"{v:.{p}f}" for v in values)
        elif isinstance(values[0], str):
            value = ",".join(f"'{v}'" for v in values)
        else:
            raise Exception(f"Parameter.to_fds: Unknown value type '{value}'")
        return "=".join((self.fds_label, value))


class Namelist(Parameter):
    """Generic FDS namelist."""

    fds_label = None
    param_cls = tuple()  # tuple of required params classes

    # Allowed indexes for specific parameters
    bf_xb_idxs, bf_xyz_idxs, bf_pb_idxs, bf_id_suffix_idxs = None, None, None, None

    def draw(self, context, layout) -> "layout":
        """Draw self UI on layout."""
        el = self.element
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        col.active = self.exported
        for p in self.param_cls:
            p(el).draw(context, col)
        return col

    @property
    def exported(self) -> "bool":
        """Get if parameter is exported."""
        return bool(getattr(self.element, str(self.bpy_export), True))

    def to_fds(self, context) -> "str or None":
        """Get FDS exported string and msg."""
        self.check(context)
        # Check if export requested
        if not self.exported:
            return
        # Init
        fds_label, params, msgs = self.fds_label, list(), list()
        for p in self.param_cls:
            to_fds = p(self.element).to_fds(context)
            if to_fds:
                if isinstance(to_fds, str):
                    params.append(to_fds)
                else:
                    params.append(to_fds[0])
                    msgs.append(to_fds[1])
        # Params
        params, mparam = [p for p in params if p], None  # rm None
        for i, p in enumerate(params):
            if not isinstance(p, str):  # find mparam
                mparam = params.pop(i)  # pop mparam
                params.pop(0)  # rm ID, managed by mparam
                break
        start = "&{} ".format(fds_label)  # get namelist w initial sep
        end = " /"  # set namelist closure w final separator, eg "\n/" or " /"
        params = separator.join(params)  # join unmutable params
        if mparam:  # multi param, eg XB
            body = "\n".join(
                "".join((start, separator.join((mp, params)), end)) for mp in mparam
            )
        else:  # no nulti param
            body = "".join((start, params, end))
        # Messages
        msg = "".join(comment(m) for m in msgs if m)
        return "".join((msg, body))


class PString(Parameter):
    """Helper for FDS string parameter."""

    bpy_prop = StringProperty

    def check(self, context):
        value = self.value
        if value and not re.match(r"^[\w\-. ]+$", value):
            raise BFException(self, "Special characters not allowed in string")


class PFYI(PString):
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


class POthers(Parameter):
    """Helper for FDS other parameters."""

    label = "Other parameters"
    description = "Other parameter (eg. PROP='Example')"
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
        return ccollection and separator.join(
            p.name for p in ccollection if p.bf_export and p.name
        )
