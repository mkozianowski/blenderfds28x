"""BlenderFDS, custom UIlist operators."""

import bpy

from bpy.types import Operator

# Operator helpers


class _OPSlotAdd:
    """Add slot to custom list operator."""

    bl_label = "Add"
    bl_description = "Add slot"

    bpy_type_name = "object"  # or 'material', 'scene'
    idx_idname = "bf_others_idx"
    bpy_idname = "bf_others"

    def set_item(self, context, item):
        """Set item in the new slot."""
        pass

    def execute(self, context):
        """Add slot and set item."""
        celem = getattr(context, self.bpy_type_name)
        ccollection = getattr(celem, self.bpy_idname)
        item = ccollection.add()
        self.set_item(context, item)
        setattr(celem, self.idx_idname, len(ccollection) - 1)
        return {"FINISHED"}


class _OPSlotRm:
    """Remove slot from custom list operator."""

    bl_label = "Remove"
    bl_description = "Remove slot"

    bpy_type_name = "object"  # or 'material', 'scene'
    idx_idname = "bf_others_idx"
    bpy_idname = "bf_others"

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type_name)
        return getattr(celem, cls.bpy_idname)

    def invoke(self, context, event):
        celem = getattr(context, self.bpy_type_name)
        cidx = getattr(celem, self.idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        if cidx < 0:  # available item?
            return {"FINISHED"}
        ccollection.remove(cidx)
        setattr(celem, self.idx_idname, max(0, cidx - 1))
        return {"FINISHED"}


class _OPSlotMv:
    """Move slot from custom list operator."""

    bl_label = "Move"
    bl_description = "Move slot"

    bpy_type_name = "object"  # or 'material', 'scene'
    idx_idname = "bf_others_idx"
    bpy_idname = "bf_others"

    direction: bpy.props.EnumProperty(items=(("UP", "Up", ""), ("DOWN", "Down", "")))

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type_name)
        return getattr(celem, cls.bpy_idname)

    def execute(self, context):
        celem = getattr(context, self.bpy_type_name)
        cidx = getattr(celem, self.idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        delta = -1 if self.direction == "UP" else 1
        neighbor = cidx + delta
        ccollection.move(neighbor, cidx)
        setattr(celem, self.idx_idname, max(0, min(neighbor, len(ccollection) - 1)))
        return {"FINISHED"}


def _get_op(bpy_type, bpy_idname, idx_idname, op_type="add"):
    bpy_type_name = bpy_type.__name__.lower()  # object
    op_name = f"{bpy_type_name.upper()}_OT_{bpy_idname}_slot_{op_type}"
    op_idname = f"{bpy_type_name}.{bpy_idname}_slot_{op_type}"
    op_template = {"add": _OPSlotAdd, "rm": _OPSlotRm, "mv": _OPSlotMv}[op_type]
    return type(
        op_name,
        (op_template, Operator),
        {
            "bl_idname": op_idname,
            "bpy_type_name": bpy_type_name,
            "idx_idname": idx_idname,
            "bpy_idname": bpy_idname,
        },
    )


def get_ops(bpy_type, bpy_idname, idx_idname):
    return (
        _get_op(bpy_type, bpy_idname, idx_idname, op_type="add"),
        _get_op(bpy_type, bpy_idname, idx_idname, op_type="rm"),
        _get_op(bpy_type, bpy_idname, idx_idname, op_type="mv"),
    )


def draw_ops(context, layout, op_idname):
    col = layout.column(align=True)
    col.operator(f"{op_idname}_slot_add", icon="ADD", text="")
    col.operator(f"{op_idname}_slot_rm", icon="REMOVE", text="")
    col.separator()
    col.operator(f"{op_idname}_slot_mv", icon="TRIA_UP", text="").direction = "UP"
    col.operator(f"{op_idname}_slot_mv", icon="TRIA_DOWN", text="").direction = "DOWN"

