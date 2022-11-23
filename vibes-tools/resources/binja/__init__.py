import re

from binaryninja import *

import binaryninjaui

from binaryninjaui import (
  Menu,
  UIAction,
  UIActionHandler,
  getMonospaceFont,
)

from PySide6.QtWidgets import (
  QFormLayout,
  QHBoxLayout,
  QLineEdit,
  QPlainTextEdit,
  QGroupBox,
  QListWidget,
  QTabWidget,
  QLabel,
  QSplitter,
  QDialog,
  QTreeWidget,
  QTreeWidgetItem,
  QVBoxLayout,
  QWidget,
  QPushButton,
  QComboBox,
)

from PySide6.QtCore import Qt
from PySide6.QtGui import QPalette

def eprint(*args, **kwargs):
  print(*args, file=sys.stderr, **kwargs)

OGRE_DECLS = """
(declare arch (name str))\n
(declare bits (size int))\n
(declare base-address (addr int))\n
(declare entry-point (addr int))\n
(declare is-little-endian (flag bool))\n
(declare mapped (addr int) (size int) (off int))\n
(declare code-region (addr int) (size int) (off int))\n
(declare named-region (addr int) (size int) (name str))\n
(declare segment (addr int) (size int) (r bool) (w bool) (x bool))\n
(declare section (addr int) (size int))\n
(declare code-start (addr int))\n
(declare named-symbol (addr int) (name str))\n
(declare symbol-chunk (addr int) (size int) (root int))
"""


def addr_to_off_seg(seg, addr):
  return (addr - seg.start) + seg.data_offset

def addr_to_off(bv, addr):
  return addr_to_off_seg(bv.get_segment_at(addr), addr)


class OGREAddrSizeOff:
  def __init__(self, name, addr, size, off):
    self.name = name
    self.addr = addr
    self.size = size
    self.off = off

  def __str__(self):
    return "(%s (addr 0x%x) (size 0x%x) (off 0x%x))" % \
      (self.name, self.addr, self.size, self.off)


class OGREMapped(OGREAddrSizeOff):
  def __init__(self, addr, size, off):
    super(OGREMapped, self).__init__("mapped", addr, size, off)


class OGRECodeRegion(OGREAddrSizeOff):
  def __init__(self, addr, size, off):
    super(OGRECodeRegion, self).__init__("code-region", addr, size, off)


class OGRENamedRegion:
  def __init__(self, addr, size, name):
    self.addr = addr
    self.size = size
    self.name = name

  def __str__(self):
    return "(named-region (addr 0x%x) (size 0x%x) (name %s))" % \
      (self.addr, self.size, self.name)


class OGRESegment(OGREAddrSizeOff):
  def __init__(self, addr, size, off):
    super(OGRESegment, self).__init__("segment", addr, size, off)


class OGRESymbolChunk:
  def __init__(self, addr, size, root):
    self.addr = addr
    self.size = size
    self.root = root

  def __str__(self):
    return "(symbol-chunk (addr 0x%x) (size 0x%x) (root 0x%x))" % \
      (self.addr, self.size, self.root)


class OGREFunction:
  def __init__(self, bv, f):
    self.name = f.name
    self.addr = f.start
    self.off = addr_to_off(bv, f.start)
    self.mapped = []
    self.code_regions = []
    self.named_regions = []
    self.symbol_chunks = []
    for r in f.address_ranges:
      name = "%s@%x" % (f.name, r.start)
      size = r.end - r.start
      off = addr_to_off(bv, r.start)
      self.mapped.append(OGREMapped(r.start, size, off))
      self.code_regions.append(OGRECodeRegion(r.start, size, off))
      self.named_regions.append(OGRENamedRegion(r.start, size, name))
      self.symbol_chunks.append(OGRESymbolChunk(r.start, size, f.start))

  def __str__(self):
    return "\n".join([
      "\n".join(map(lambda x: str(x), self.mapped)),
      "\n".join(map(lambda x: str(x), self.code_regions)),
      "\n".join(map(lambda x: str(x), self.named_regions)),
      "(code-start (addr 0x%x))" % self.addr,
      "(named-symbol (addr 0x%x) (name %s))" % (self.addr, self.name),
      "\n".join(map(lambda x: str(x), self.symbol_chunks))
    ])


class OGREData(OGREAddrSizeOff):
  def __init__(self, addr, size, off):
    super(OGREData, self).__init__("", addr, size, off)
    self.refs = 1

  def __str__(self):
    mapped = OGREMapped(self.addr, self.size, self.off)
    segment = OGRESegment(self.addr, self.size, self.off)
    return "\n".join([str(mapped), str(segment)])


def rodata_of_func(bv, f):
  result = {}
  for l in f.llil_instructions:
    for r in bv.get_code_refs_from(l.address):
      if bv.get_functions_containing(r):
        continue
      seg = bv.get_segment_at(r)
      if seg is None or seg.writable:
        continue
      d = bv.get_data_var_at(r)
      if d is None:
        continue
      size = d.type.width
      if size <= 0:
        continue
      off = addr_to_off_seg(seg, r)
      result[r] = OGREData(r, size, off)
  return result

def get_ogre_functions(bv):
  try:
    fs = bv.query_metadata("vibes.ogre-functions")
  except KeyError:
    fs = []
    bv.store_metadata("vibes.ogre-functions", fs)
  return fs

def save_ogre_func(bv, f):
  fs = bv.query_metadata("vibes.ogre-functions")
  fs.append(f)
  fs = list(set(fs))
  bv.store_metadata("vibes.ogre-functions", fs)

def remove_ogre_func(bv, f):
  fs = bv.query_metadata("vibes.ogre-functions")
  try:
    fs.remove(f)
    bv.store_metadata("vibes.ogre-functions", fs)
  except ValueError:
    pass


class OGRE:
  def __init__(self, bv):
    self.bv = bv
    if bv.arch.name.startswith("thumb2"):
      self.arch = "thumb"
    elif bv.arch.name.startswith("armv7"):
      self.arch = "arm"
    else:
      assert False
    self.bits = bv.address_size * 8
    self.base_address = bv.start
    self.is_little_endian = bv.endianness == Endianness.LittleEndian
    self.entry_point = bv.entry_point
    if self.arch == "thumb":
      self.entry_point = self.entry_point & ~1
    self.functions = {}
    self.rodata = {}

  def add_function(self, f):
    if f.start in self.functions:
      return False
    self.functions[f.start] = OGREFunction(self.bv, f)
    for r, d in rodata_of_func(self.bv, f).items():
      if r in self.rodata:
        self.rodata[r].refs += 1
      else:
        self.rodata[r] = d
    return True

  def delete_function(self, f):
    remove = set()
    rodata = rodata_of_func(self.bv, f)
    for r, d in self.rodata.items():
      if r in rodata:
        d.refs -= 1
        if d.refs < 1:
          remove.add(r)
    for r in remove:
      del self.rodata[r]
    del self.functions[f.start]

  def __str__(self):
    le = "true" if self.is_little_endian else "false"
    required = "\n".join([
      "(arch (name %s))" % self.arch,
      "(bits (size %d))" % self.bits,
      "(base-address (addr 0x%x))" % self.base_address,
      "(is-little-endian (flag %s))" % le,
      "(entry-point (addr 0x%x))" % self.entry_point
    ])
    entries = "\n\n".join([
      "\n\n".join(map(lambda x: str(x), list(self.functions.values()))),
      "\n\n".join(map(lambda x: str(x), list(self.rodata.values())))
    ])
    return "\n\n".join([OGRE_DECLS, required, entries])


# The main window for holding information about the OGRE spec.
class OGREEditor(QWidget):
  def __init__(self, context, parent=None):
    super(OGREEditor, self).__init__(parent)

    self.data = context.binaryView

    self.setWindowTitle("VIBES OGRE Editor")
    self.currentOffset = 0

    self.container = QWidget(parent)
    list_layout = QHBoxLayout()

    self.ogre = OGRE(self.data)
    self.functions = {}

    self.available_funcs_widget = QListWidget(self.container)
    add_function_button = QPushButton("Add function", self.container)
    add_function_button.clicked.connect(self._add_function)
    available_funcs_layout = QVBoxLayout()
    available_funcs_layout.addWidget(self.available_funcs_widget)
    available_funcs_layout.addWidget(add_function_button)

    self.lifted_funcs_widget = QListWidget(self.container)
    remove_function_button = QPushButton("Remove function", self.container)
    remove_function_button.clicked.connect(self._remove_function)
    lifted_funcs_layout = QVBoxLayout()
    lifted_funcs_layout.addWidget(self.lifted_funcs_widget)
    lifted_funcs_layout.addWidget(remove_function_button)

    self._update_functions()

    available_funcs_group = QGroupBox("Available functions", self.container)
    available_funcs_group.setLayout(available_funcs_layout)

    lifted_funcs_group = QGroupBox("Functions to lift", self.container)
    lifted_funcs_group.setLayout(lifted_funcs_layout)

    functions_layout = QHBoxLayout()
    functions_layout.addWidget(available_funcs_group)
    functions_layout.addWidget(lifted_funcs_group)

    functions_widget = QWidget(self.container)
    functions_widget.setLayout(functions_layout)

    update_funcs_button = QPushButton("Update functions", self.container)
    update_funcs_button.clicked.connect(self._update_functions)

    for name in get_ogre_functions(self.data):
      if not self._add_function_by_name(name, save=False):
        remove_ogre_func(self.data, name)

    layout = QVBoxLayout()
    layout.addWidget(functions_widget)
    layout.addWidget(update_funcs_button)
    self.setLayout(layout)

  def _update_functions(self):
    self.available_funcs_widget.clear()
    for s in self.data.get_symbols():
      if s.auto or s.type != SymbolType.FunctionSymbol:
        continue
      f = self.data.get_functions_containing(s.address)[0]
      self.functions[f.name] = f
      self.available_funcs_widget.addItem(f.name)
    to_remove = []
    for row in range(self.lifted_funcs_widget.count()):
      item = self.lifted_funcs_widget.item(row)
      if item.text() not in self.functions:
        to_remove.append(item)
    for item in to_remove:
      row = self.lifted_funcs_widget.row(item)
      self.lifted_funcs_widget.takeItem(row)

  def _add_function(self):
    item = self.available_funcs_widget.currentItem()
    if item is None:
      return
    name = item.text()
    self._add_function_by_name(name)

  def _add_function_by_name(self, name, save=True):
    f = self.functions.get(name)
    if f is None:
      return False
    if self.ogre.add_function(f):
      self.lifted_funcs_widget.addItem(name)
      if save:
        save_ogre_func(self.data, name)
      return True
    else:
      return False

  def _remove_function(self):
    item = self.lifted_funcs_widget.currentItem()
    if item is None:
      return
    name = item.text()
    if name not in self.functions:
      return
    f = self.functions[name]
    self.ogre.delete_function(f)
    row = self.lifted_funcs_widget.row(item)
    self.lifted_funcs_widget.takeItem(row)
    remove_ogre_func(self.data, name)


# Higher-level variables
class HigherVar:
  REG_VAR = 0
  FRAME_VAR = 1
  GLOBAL_VAR = 2
  FUNCTION_VAR = 3

  def __init__(self, name, value, type):
    self.name = name
    self.value = value
    self.type = type

  def type_str(self):
    if self.type == HigherVar.REG_VAR:
      return "Register"
    elif self.type == HigherVar.FRAME_VAR:
      return "Frame"
    elif self.type == HigherVar.GLOBAL_VAR:
      return "Global"
    elif self.type == HigherVar.FUNCTION_VAR:
      return "Function"
    else:
      assert False

  def value_str(self):
    if self.type == HigherVar.REG_VAR:
      return self.value
    elif self.type == HigherVar.FRAME_VAR:
      sp, off = self.value
      if off < 0:
        return "[%s - 0x%x]" % (sp, -off)
      elif off > 0:
        return "[%s + 0x%x]" % (sp, off)
      else:
        return "[%s]" % sp
    elif self.type == HigherVar.GLOBAL_VAR:
      return "0x%x" % self.value
    elif self.type == HigherVar.FUNCTION_VAR:
      return "0x%x" % self.value
    else:
      assert False


# Patch variables
class PatchVar(HigherVar):
  def __init__(self, name, value, type, default_reg=True):
    if type == HigherVar.REG_VAR and default_reg:
      value = (value, value)
    super(PatchVar, self).__init__(name, value, type)

  def set_at_entry(self, r):
    if self.type == HigherVar.REG_VAR:
      self.value = (r, self.value[1])

  def set_at_exit(self, r):
    if self.type == HigherVar.REG_VAR:
      self.value = (self.value[0], r)

  def value_str(self):
    # not valid for PatchVar
    assert False

  def serialize(self, bv):
    storage = self._serialize_storage_class(bv)
    return {"name": self.name, "storage-class": storage}

  def _serialize_storage_class(self, bv):
    sz = bv.arch.address_size * 8
    if self.type == HigherVar.REG_VAR:
      reg = {}
      if self.value[0] is not None:
        reg["at-entry"] = self.value[0]
      if self.value[1] is not None:
        reg["at-exit"] = self.value[1]
      return ["register", reg]
    elif self.type == HigherVar.FRAME_VAR:
      reg, off_int = self.value
      if off_int < 0:
        off = "-0x%x:%d" % (-off_int, sz)
      else:
        off = "0x%x:%d" % (off_int, sz)
      return ["memory", ["frame", reg, off]]
    elif self.type == HigherVar.GLOBAL_VAR:
      return ["memory", ["address", "0x%x:%d" % (self.value, sz)]]
    elif self.type == HigherVar.FUNCTION_VAR:
      return ["constant", "0x%x:%d" % (self.value, sz)]
    else:
      assert False

  @staticmethod
  def deserialize(d):
    name = d["name"]
    type, value = PatchVar._deserialize_storage_class(d["storage-class"])
    return PatchVar(name, value, type, default_reg=False)

  @staticmethod
  def _deserialize_storage_class(d):
    if d[0] == "register":
      type = HigherVar.REG_VAR
      value = (d[1].get("at-entry"), d[1].get("at-exit"))
    elif d[0] == "memory":
      m = d[1]
      if m[0] == "frame":
        type = HigherVar.FRAME_VAR
        value = m[1], int(m[2].split(":")[0], base=16)
      elif m[0] == "address":
        type = HigherVar.GLOBAL_VAR
        value = int(m[1].split(":")[0], base=16)
      else:
        assert False
    elif d[0] == "constant":
      type = HigherVar.FUNCTION_VAR
      value = int(d[1].split(":")[0], base=16)
    else:
      assert False
    return (type, value)


# As a general rule of thumb, the stack pointer needs to be
# aligned at a double word size when entering any call site.
def sp_alignment(bv):
  return bv.address_size * 2

# Maximum range for a "frame" offset.
def frame_range(bv):
  a = bv.arch.name
  if a == "thumb2" or a == "thumb2eb":
    return -255, 4095
  elif a == "armv7" or a == "armv7eb":
    return -4095, 4095
  else:
    assert False

THUMB2_REGS = [
  "R0", "R1", "R2", "R3",
  "R4", "R5", "R6", "R7"
]

ARMv7_REGS = [
  "R0", "R1", "R2",  "R3",
  "R4", "R5", "R6",  "R7",
  "R8", "R9", "R10", "R11",
  "R12"
]

def available_regs(bv, include_sp=False):
  a = bv.arch.name
  if a == "thumb2" or a == "thumb2eb":
    l = THUMB2_REGS.copy()
    if include_sp:
      l.append("SP")
    return l
  elif a == "armv7" or a == "armv7eb":
    l = ARMv7_REGS.copy()
    if include_sp:
      l.append("SP")
    return l
  else:
    assert False


# Metadata for a patch.
class PatchInfo:
  def __init__(self, name, addr, size, bv):
    self.name = name
    self.addr = addr
    self.size = size
    self.vars = {}
    if bv is None:
      self.sp_align = 0
    else:
      f = self.function(bv)
      l = f.get_low_level_il_at(addr)
      sp = l.get_reg_value(bv.arch.stack_pointer)
      if sp.type == RegisterValueType.StackFrameOffset:
        a = sp_alignment(bv)
        self.sp_align = sp.value % a
      else:
        self.sp_align = 0

  def end_addr(self):
    return self.addr + self.size

  def function(self, bv):
    return bv.get_functions_containing(self.addr)[0]

  def set_var(self, name, value, type):
    self.vars[name] = PatchVar(name, value, type)

  def delete_var(self, name):
    del self.vars[name]

  def serialize(self, bv):
    sz = bv.arch.address_size * 8
    point = "0x%x:%d" % (self.addr, sz)
    vars = map(lambda x: x.serialize(bv), self.vars.values())
    return {
      "patch-point": point,
      "patch-size": self.size,
      "sp-align": self.sp_align,
      "patch-vars": list(vars)
    }

  @staticmethod
  def deserialize(name, d):
    addr = int(d["patch-point"].split(":")[0], base=16)
    size = d["patch-size"]
    p = PatchInfo(name, addr, size, None)
    p.sp_align = d["sp-align"]
    vars = map(lambda x: PatchVar.deserialize(x), d["patch-vars"])
    for v in vars:
      p.vars[v.name] = v
    return p

  def collect_higher_vars(self, bv):
    end = self.end_addr()
    f = self.function(bv)
    lstart = f.get_low_level_il_at(self.addr)
    lend = f.get_low_level_il_at(end)
    bb_patch_ll = f.llil.get_basic_block_at(lend.instr_index)
    result = {}

    def add(name, h):
      if name not in result:
        result[name] = [h]
      else:
        result[name].append(h)

    def defined_after_patch(v):
      for d in f.hlil.get_var_definitions(v):
        if d.address == end:
          return True
      return False

    # Is basic block `dst` reachable from `src`?
    def reachable(src, dst, src_addr, dst_addr):
      if src.start == dst.start:
        # Special case for when the instructions are in the
        # same basic block.
        return dst_addr >= src_addr
      elif src in dst.strict_dominators:
        return True
      elif dst in src.dominance_frontier:
        return True
      else:
        return False

    # Check for variables that were spilled to the stack.
    def spilled(v):
      bb_patch = f.mlil.get_basic_block_at(lend.mlil.instr_index)
      for u in f.mlil.get_var_uses(v):
        bb_use = f.mlil.get_basic_block_at(u.instr_index)
        if not reachable(bb_use, bb_patch, u.address, self.addr):
          continue
        # Are we defining a stack variable?
        if not isinstance(u, MediumLevelILSetVar):
          continue
        d = u.dest
        if isinstance(d, Variable) and \
           d.source_type == VariableSourceType.StackVariableSourceType:
          return d.storage
      return None

    # The stack offsets in the BN Variable class need to be adjusted
    # for the changes in the SP at the patch point. We will ignore
    # our `sp_align` attribute for now, since this is only needed
    # if the patch includes function calls.
    def stack_frame(s):
      sp = bv.arch.stack_pointer
      sp_val = lstart.get_reg_value(sp)
      if sp_val.type == RegisterValueType.StackFrameOffset:
        off = sp_val.value
      else:
        off = 0
      return (sp.upper(), s - off)

    # Is register `r` live at the patch site?
    def reg_live(r):
      for ssa in f.llil.ssa_regs:
        if ssa.reg != r:
          continue
        # If this is `None` then the register was passed as an argument.
        d = f.llil.get_ssa_reg_definition(ssa)
        if d is not None:
          bb_def_ll = f.llil.get_basic_block_at(d.instr_index)
          if not reachable(bb_def_ll, bb_patch_ll, d.address, self.addr):
            continue
        for u in f.llil.get_ssa_reg_uses(ssa):
          bb_use_ll = f.llil.get_basic_block_at(u.instr_index)
          if reachable(bb_patch_ll, bb_use_ll, self.addr, u.address):
            return True
      return False

    # Collect the live HLIL variables.
    i = lend.mlil.hlil.instr_index
    for v in f.hlil.vars:
      # Disregard this variable if it was defined within the patch
      # region, or at the very end.
      if defined_after_patch(v):
        continue
      # XXX: should we consider all variables that can be accessed
      # from this point, not just those that are live at the end
      # of the patch?
      if not f.hlil.is_var_live_at(v, i):
        continue
      if v.source_type == VariableSourceType.RegisterVariableSourceType:
        s = spilled(v)
        if s is not None:
          frame = stack_frame(s)
          add(v.name, HigherVar(v.name, frame, HigherVar.FRAME_VAR))
        else:
          r = bv.arch.get_reg_name(v.storage).upper()
          add(v.name, HigherVar(v.name, r, HigherVar.REG_VAR))
      elif v.source_type == VariableSourceType.StackVariableSourceType:
        frame = stack_frame(v.storage)
        add(v.name, HigherVar(v.name, frame, HigherVar.FRAME_VAR))

    # Relate known register values at the patch site with known
    # data symbols.
    possible_frames = []
    for r in f.llil.regs:
      v = lstart.get_reg_value(r)
      if v.type == RegisterValueType.ConstantPointerValue:
        s = bv.get_symbol_at(v.value)
        if s and s.type == SymbolType.DataSymbol:
          reg = r.name.upper()
          possible_frames.append((reg, v.value))
          if reg_live(r):
            add(s.name, HigherVar(s.name, reg, HigherVar.REG_VAR))

    # Grab all the known function and data symbols. We should
    # ignore those which were automatically named, to avoid
    # cluttering the output.
    flow, fhigh = frame_range(bv)
    for s in bv.get_symbols():
      if s.auto:
        continue
      if s.type == SymbolType.DataSymbol:
        a = s.address
        add(s.name, HigherVar(s.name, a, HigherVar.GLOBAL_VAR))
        # See if this symbol can be accessed via frame through a
        # register with a known value.
        for reg, val in possible_frames:
          off = a - val
          if (a < val and off >= flow) or (a > val and off <= fhigh):
            add(s.name, HigherVar(s.name, (reg, off), HigherVar.FRAME_VAR))
      elif s.type == SymbolType.FunctionSymbol:
        add(s.name, HigherVar(s.name, s.address, HigherVar.FUNCTION_VAR))

    return result


var_name_re = re.compile("[a-zA-Z_][a-zA-Z0-9_]*")

def valid_var_name(name):
  return var_name_re.fullmatch(name) is not None

def prompt_var_name(vars):
  while True:
    name = get_text_line_input("Please provide a variable name",
                               "VIBES: new variable")
    if name is None:
      return None
    name = name.decode('ASCII')
    if name is None:
      return None
    elif name in vars:
      eprint("A variable with the name '%s' already exists" % name)
    elif not valid_var_name(name):
      eprint("Invalid variable name '%s'" % name)
    else:
      return name

patches = {}

def get_patches(bv):
  global patches
  try:
    ps = bv.query_metadata("vibes.patch-infos")
  except KeyError:
    ps = {}
    bv.store_metadata("vibes.patch-infos", ps)
  if not patches:
    for k, v in ps.items():
      patches[k] = PatchInfo.deserialize(k, v)
  return patches

def save_patch(bv, p):
  ps = bv.query_metadata("vibes.patch-infos")
  ps[p.name] = p.serialize(bv)
  bv.store_metadata("vibes.patch-infos", ps)

def delete_patch(bv, name):
  ps = bv.query_metadata("vibes.patch-infos")
  del ps[name]
  bv.store_metadata("vibes.patch-infos", ps)
  ps = bv.query_metadata("vibes.patch-codes")
  del ps[name]
  bv.store_metadata("vibes.patch-codes", ps)

def get_patch_code(bv, name):
  try:
    ps = bv.query_metadata("vibes.patch-codes")
  except KeyError:
    ps = {name: ""}
    bv.store_metadata("vibes.patch-codes", ps)
  return ps[name]

def save_patch_code(bv, name, code):
  ps = bv.query_metadata("vibes.patch-codes")
  ps[name] = code
  bv.store_metadata("vibes.patch-codes", ps)


# Container class for the GUI elements for editing a patch.
class PatchView:
  def __init__(self, name, bv, addr, size, parent=None):
    self.idx = 0
    self.name = name
    self.bv = bv

    patches = get_patches(bv)
    if name not in patches:
      p = PatchInfo(name, addr, size, bv)
      patches[name] = p
      save_patch(bv, p)
    else:
      p = patches[name]

    self.hex_var_widget = {}

    self.info_widget = QWidget(parent)
    info_layout = QFormLayout()

    self.function_label = QLabel(p.function(bv).name, self.info_widget)
    info_layout.addRow("Function", self.function_label)

    self.patch_point_widget = QLineEdit(("0x%x" % addr), self.info_widget)
    self.patch_point_widget.editingFinished.connect(self._update_patch_point)
    info_layout.addRow("Patch point", self.patch_point_widget)

    self.patch_size_widget = QLineEdit(("%d" % size), self.info_widget)
    self.patch_size_widget.editingFinished.connect(self._update_patch_size)
    info_layout.addRow("Patch size", self.patch_size_widget)

    self.sp_align_widget = QLineEdit(("%d" % 0), self.info_widget)
    self.sp_align_widget.editingFinished.connect(self._update_sp_align)
    info_layout.addRow("SP adjustment", self.sp_align_widget)

    self.patch_vars_widget = QTreeWidget(self.info_widget)
    self.patch_vars_widget.setColumnCount(2)
    self.patch_vars_widget.setHeaderLabels(["Variable", "Value"])
    info_layout.addRow("Patch variables", self.patch_vars_widget)

    patch_vars_buttons = QWidget(self.info_widget)
    patch_vars_buttons_layout = QHBoxLayout()
    new_var_button = QPushButton("New variable", patch_vars_buttons)
    new_var_button.clicked.connect(self._new_var)
    delete_var_button = QPushButton("Delete variable", patch_vars_buttons)
    delete_var_button.clicked.connect(self._delete_var)
    patch_vars_buttons_layout.addWidget(new_var_button)
    patch_vars_buttons_layout.addWidget(delete_var_button)
    patch_vars_buttons.setLayout(patch_vars_buttons_layout)
    for v in p.vars.values():
      self._add_var_impl(v.name, v.value, v.type)
    info_layout.addWidget(patch_vars_buttons)

    self.higher_vars_widget = QTreeWidget(self.info_widget)
    self.higher_vars_widget.setColumnCount(2)
    self.higher_vars_widget.setHeaderLabels(["Variable", "Value"])
    self._refresh_higher_vars()
    info_layout.addRow("Live variables", self.higher_vars_widget)

    higher_vars_buttons = QWidget(self.info_widget)
    higher_vars_buttons_layout = QHBoxLayout()
    add_higher_var_button = \
      QPushButton("Add live variable", higher_vars_buttons)
    add_higher_var_button.clicked.connect(self._add_higher_var)
    higher_vars_buttons_layout.addWidget(add_higher_var_button)
    refresh_higher_vars_button = \
      QPushButton("Refresh live variables", higher_vars_buttons)
    refresh_higher_vars_button.clicked.connect(self._refresh_higher_vars)
    higher_vars_buttons_layout.addWidget(refresh_higher_vars_button)
    higher_vars_buttons.setLayout(higher_vars_buttons_layout)
    info_layout.addWidget(higher_vars_buttons)

    self.info_widget.setLayout(info_layout)

    self.code_widget = QWidget(parent)
    code_widget_layout = QVBoxLayout()

    self.code_edit = QPlainTextEdit(get_patch_code(bv, name), self.code_widget)
    self.code_edit.setFont(getMonospaceFont(self.code_edit))
    self.code_edit.setTabStopDistance(20)
    code_widget_layout.addWidget(self.code_edit)

    code_edit_save = QPushButton("Save code", self.code_widget)
    code_edit_save.clicked.connect(self._save_code)
    code_widget_layout.addWidget(code_edit_save)

    self.code_widget.setLayout(code_widget_layout)

  def add_to_widget(self, tab_widget):
    tab_widget.addTab(self.info_widget, "Info")
    tab_widget.addTab(self.code_widget, "C Code")
    tab_widget.setCurrentIndex(self.idx)

  def c_code(self):
    return self.code_edit.document().toPlainText()

  def _save_code(self):
    save_patch_code(self.bv, self.name, self.c_code())

  def current_patch(self):
    patches = get_patches(self.bv)
    return patches[self.name]

  def _refresh_higher_vars(self):
    p = self.current_patch()
    self.higher_vars_widget.clear()
    self.hvars = p.collect_higher_vars(self.bv)
    for name, vs in self.hvars.items():
      var = QTreeWidgetItem(self.higher_vars_widget)
      var.setText(0, name)
      for h in vs:
        storage = QTreeWidgetItem(var)
        storage.setText(0, h.type_str())
        storage.setText(1, h.value_str())
        var.addChild(storage)
      self.higher_vars_widget.addTopLevelItem(var)

  def _update_patch_point(self):
    p = self.current_patch()
    old = p.addr
    text = self.patch_point_widget.text()
    try:
      p.addr = int(text)
      f = p.function(self.bv)
      self.function_label.setText(f.name)
      self._refresh_higher_vars()
      self.patch_point_widget.setText("0x%x" % p.addr)
      save_patch(self.bv, p)
    except Exception:
      eprint("Invalid patch point: " + text)
      p.addr = old
      self.patch_point_widget.setText("0x%x" % old)

  def _update_patch_size(self):
    p = self.current_patch()
    old = p.size
    text = self.patch_size_widget.text()
    try:
      p.size = int(text)
      assert p.size >= 0
      self._refresh_higher_vars()
      self.patch_size_widget.setText("%d" % p.size)
      save_patch(self.bv, p)
    except Exception:
      eprint("Invalid patch size: " + text)
      p.size = old
      self.patch_size_widget.setText("%d" % old)

  def _update_sp_align(self):
    p = self.current_patch()
    old = p.sp_align
    text = self.sp_align_widget.text()
    try:
      p.sp_align = int(text)
      self.sp_align_widget.setText("%d" % p.sp_align)
      save_patch(self.bv, p)
    except Exception:
      eprint("Invalid SP adjustment: " + text)
      p.sp_align = old
      self.sp_align_widget.setText("%d" % old)

  def _new_var(self):
    p = self.current_patch()
    name = prompt_var_name(p.vars)
    if name is not None:
      self._add_var(p, name)

  def _add_var(self, p, name, hvar=None):
    value, type = (None, HigherVar.REG_VAR) \
      if hvar is None else (hvar.value, hvar.type)
    p.vars[name] = PatchVar(name, value, type)
    self._add_var_impl(name, value, type)
    save_patch(self.bv, p)

  def _add_var_impl(self, name, value, type):
    var = QTreeWidgetItem(self.patch_vars_widget)
    var.setText(0, name)
    type_combo = QComboBox(self.patch_vars_widget)
    type_combo.addItem("Register", HigherVar.REG_VAR)
    type_combo.addItem("Frame", HigherVar.FRAME_VAR)
    type_combo.addItem("Global", HigherVar.GLOBAL_VAR)
    type_combo.addItem("Function", HigherVar.FUNCTION_VAR)
    type_combo.setCurrentIndex(type)
    type_combo.currentIndexChanged.connect(self._var_type_changed)
    self._add_var_type(var, value, type)
    self.patch_vars_widget.setItemWidget(var, 1, type_combo)
    self.patch_vars_widget.addTopLevelItem(var)

  def _add_higher_var(self):
    item = self.higher_vars_widget.currentItem()
    var = item.parent()
    if var is None:
      eprint("No higher var value is selected")
      return
    name = var.text(0)
    p = self.current_patch()
    if name in p.vars:
      eprint("Variable %s already exists in the patch variables" % name)
      return
    hvar = self.hvars[name][var.indexOfChild(item)]
    self._add_var(p, name, hvar=hvar)

  def _add_var_type(self, var, value, type):
    name = var.text(0)
    if type == HigherVar.REG_VAR:
      regs = available_regs(self.bv)
      if isinstance(value, str):
        try:
          ientry = regs.index(value) + 1
          iexit = ientry
        except ValueError:
          ientry = 0
          iexit = 0
      elif isinstance(value, tuple) and len(value) == 2:
        try:
          ientry = regs.index(value[0]) + 1
          iexit = regs.index(value[1]) + 1
        except ValueError:
          ientry = 0
          iexit = 0
      else:
        ientry = 0
        iexit = 0
      at_entry_combo = QComboBox(self.patch_vars_widget)
      at_entry_combo.addItem("(none)")
      at_entry_combo.addItems(regs)
      at_entry_combo.setCurrentIndex(ientry)
      at_entry_combo.currentIndexChanged.connect(self._at_entry_changed)
      at_exit_combo = QComboBox(self.patch_vars_widget)
      at_exit_combo.addItem("(none)")
      at_exit_combo.addItems(regs)
      at_exit_combo.setCurrentIndex(iexit)
      at_exit_combo.currentIndexChanged.connect(self._at_exit_changed)
      at_entry = QTreeWidgetItem(var)
      at_entry.setText(0, "At entry")
      at_exit = QTreeWidgetItem(var)
      at_exit.setText(0, "At exit")
      var.addChild(at_entry)
      var.addChild(at_exit)
      self.patch_vars_widget.setItemWidget(at_entry, 1, at_entry_combo)
      self.patch_vars_widget.setItemWidget(at_exit, 1, at_exit_combo)
    elif type == HigherVar.FRAME_VAR:
      regs = available_regs(self.bv, include_sp=True)
      try:
        i = regs.index(value[0])
      except ValueError:
        i = 0
      base_combo = QComboBox(self.patch_vars_widget)
      base_combo.addItems(regs)
      base_combo.setCurrentIndex(i)
      base_combo.currentIndexChanged.connect(self._base_changed)
      offset_widget = QLineEdit(("0x%x" % value[1]), self.patch_vars_widget)
      offset_widget.editingFinished.connect(self._hex_changed)
      self.hex_var_widget[name] = offset_widget
      base = QTreeWidgetItem(var)
      base.setText(0, "Base register")
      offset = QTreeWidgetItem(var)
      offset.setText(0, "Offset")
      var.addChild(base)
      var.addChild(offset)
      self.patch_vars_widget.setItemWidget(base, 1, base_combo)
      self.patch_vars_widget.setItemWidget(offset, 1, offset_widget)
    elif type == HigherVar.GLOBAL_VAR or \
         type == HigherVar.FUNCTION_VAR:
      address_widget = QLineEdit(("0x%x" % value), self.patch_vars_widget)
      address_widget.editingFinished.connect(self._hex_changed)
      self.hex_var_widget[name] = address_widget
      address = QTreeWidgetItem(var)
      address.setText(0, "Address")
      var.addChild(address)
      self.patch_vars_widget.setItemWidget(address, 1, address_widget)
    else:
      assert False

  def _delete_var(self):
    var = self._current_var_item()
    if var is None:
      return
    name = var.text(0)
    if name in self.hex_var_widget:
      del self.hex_var_widget[name]
    p = self.current_patch()
    p.delete_var(name)
    self.patch_vars_widget.invisibleRootItem().removeChild(var)
    save_patch(self.bv, p)

  def _var_type_changed(self, type):
    var = self._current_var_item()
    if var is None:
      return
    var.takeChildren()
    p = self.current_patch()
    if type == HigherVar.REG_VAR:
      value = None
    elif type == HigherVar.FRAME_VAR:
      value = (available_regs(self.bv, include_sp=True)[0], 0)
    elif type == HigherVar.GLOBAL_VAR or \
         type == HigherVar.FUNCTION_VAR:
      value = 0
    else:
      assert False
    name = var.text(0)
    if name in self.hex_var_widget:
      del self.hex_var_widget[name]
    p.set_var(var.text(0), value, type)
    self._add_var_type(var, value, type)
    save_patch(self.bv, p)

  def _current_var_item(self):
    var = self.patch_vars_widget.currentItem()
    if var is None:
      return None
    while var.parent() is not None:
      var = var.parent()
    return var

  def _at_entry_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    reg = None if i == 0 else available_regs(self.bv)[i - 1]
    p = self.current_patch()
    p.vars[var.text(0)].set_at_entry(reg)
    save_patch(self.bv, p)

  def _at_exit_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    reg = None if i == 0 else available_regs(self.bv)[i - 1]
    p = self.current_patch()
    p.vars[var.text(0)].set_at_exit(reg)
    save_patch(self.bv, p)

  def _base_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    reg = available_regs(self.bv, include_sp=True)[i]
    p = self.current_patch
    v = self.current_patch().vars[var.text(0)]
    v.value = (reg, v.value[1])
    save_patch(self.bv, p)

  def _hex_changed(self):
    var = self._current_var_item()
    if var is None:
      return
    name = var.text(0)
    p = self.current_patch()
    v = p.vars[name]
    old = v.value[1] if v.type == HigherVar.FRAME_VAR else v.value
    widget = self.hex_var_widget[name]
    text = widget.text()
    try:
      new = int(text)
      v.value = (v.value[0], new) if v.type == HigherVar.FRAME_VAR else new
      widget.setText("0x%x" % new)
      save_patch(self.bv, p)
    except Exception:
      eprint("Invalid hex value: " + text)
      widget.setText("0x%x" % old)


# The main window for holding information about the patches.
class PatchEditor(QDialog):
  def __init__(self, context, parent=None):
    super(PatchEditor, self).__init__(parent)

    self.data = context.binaryView

    self.setWindowTitle("VIBES Patch Editor")
    self.currentOffset = 0

    self.patches = {}
    self.current_patch = None

    self.container = QWidget(parent)

    self.patch_tab_widget = QTabWidget(self.container)

    self.patch_list_widget = QListWidget(self.container)
    self.patch_list_widget.currentItemChanged.connect(self._select_patch)

    # We may have started adding patches before we constructed this
    # dialog object, so we should synchronize this data.
    patches = get_patches(self.data)
    for name, p in patches.items():
      if name not in self.patches:
        self.add_patch(name, p.addr, p.size)

    self.patch_delete_button = QPushButton("Delete", self.container)
    self.patch_delete_button.clicked.connect(self._delete_patch)

    patch_buttons_layout = QHBoxLayout()
    patch_buttons_layout.addWidget(self.patch_delete_button)

    patches_layout = QVBoxLayout()
    patches_layout.addWidget(self.patch_list_widget)
    patches_layout.addLayout(patch_buttons_layout)

    patches_widget = QWidget(self.container)
    patches_widget.setLayout(patches_layout)

    splitter = QSplitter(self.container)
    splitter.addWidget(patches_widget)
    splitter.addWidget(self.patch_tab_widget)
    splitter.setChildrenCollapsible(False)

    # Make the list widget smaller
    w = max(patches_widget.minimumSizeHint().width(),
            self.patch_tab_widget.minimumSizeHint().width())
    splitter.setSizes([int(w / 2), w + int(w / 2)])

    self.ogre = OGREEditor(context, self.container)

    tabs = QTabWidget(self.container)
    tabs.addTab(splitter, "Patches")
    tabs.addTab(self.ogre, "OGRE")

    layout = QHBoxLayout()
    layout.addWidget(tabs)

    self.setLayout(layout)

  def add_patch(self, name, addr, size):
    if name not in self.patches:
      self.patches[name] = PatchView(name, self.data, addr, size)
      self.patch_list_widget.addItem(name)
    self.current_patch = self.patches[name]
    self.current_patch.add_to_widget(self.patch_tab_widget)

  def _select_patch(self, current, previous):
    if previous:
      name = previous.text()
      if name in self.patches:
        self.patches[name].idx = self.patch_tab_widget.currentIndex()
    self.patch_tab_widget.clear()
    if current:
      name = current.text()
      self.add_patch(name, None, None)

  def _delete_patch(self):
    items = self.patch_list_widget.selectedItems()
    if not items:
      return
    for item in items:
      patches = get_patches(self.data)
      name = item.text()
      del patches[name]
      del self.patches[name]
      delete_patch(self.data, name)
      self.patch_list_widget.takeItem(self.patch_list_widget.row(item))


PATCH_EDITOR = 'VIBES\\Patch Editor'
patch_editor = None

supported = ["armv7", "thumb2", "armv7eb", "thumb2eb"]

def check_arch(bv):
  arch = bv.arch.name
  if arch not in supported:
    show_message_box("VIBES", "Unsupported architecture %s" % arch,
                     MessageBoxButtonSet.OKButtonSet,
                     MessageBoxIcon.ErrorIcon)
    return False
  return True

def launch_editor(context):
  global patch_editor
  bv = context.binaryView
  if not bv:
    show_message_box("VIBES", "No binary currently in view",
                     MessageBoxButtonSet.OKButtonSet,
                     MessageBoxIcon.ErrorIcon)
    return
  if not check_arch(bv):
    return
  if not patch_editor:
    patch_editor = PatchEditor(context, parent=context.widget)
  patch_editor.show()

UIAction.registerAction(PATCH_EDITOR)
UIActionHandler.globalActions().bindAction(PATCH_EDITOR, UIAction(launch_editor))
Menu.mainMenu('Plugins').addAction(PATCH_EDITOR, 'show')

patch_name_re = re.compile("[A-Za-z][A-Za-z-_0-9]*")

def valid_patch_name(name):
 return patch_name_re.fullmatch(name) is not None

def prompt_patch_name(bv):
  patches = get_patches(bv)
  while True:
    name = get_text_line_input("Please provide a patch name",
                               "VIBES: new patch")
    if name is None:
      return None
    name = name.decode('ASCII')
    if name is None:
      return None
    elif name in patches:
      eprint("A patch with the name '%s' already exists" % name)
    elif not valid_patch_name(name):
      eprint("Invalid patch name '%s'" % name)
    else:
      return name

def patch_range(bv, addr, n):
  if not check_arch(bv):
    return
  global patch_editor
  name = prompt_patch_name(bv)
  if name is None:
    return
  if patch_editor:
    patch_editor.add_patch(name, addr, n)
  else:
    # Patch editor hasn't been created yet
    p = PatchInfo(name, addr, n, bv)
    patches = get_patches(bv)
    patches[name] = p
    save_patch(bv, p)

def patch_addr(bv, addr):
  if not check_arch(bv):
    return
  global patch_editor
  name = prompt_patch_name(bv)
  if name is None:
    return
  if patch_editor:
    patch_editor.add_patch(name, addr, 0)
  else:
    # Patch editor hasn't been created yet
    p = PatchInfo(name, addr, 0, bv)
    patches = get_patches(bv)
    patches[name] = p
    save_patch(bv, p)

PluginCommand.register_for_range("VIBES\\Patch highlighted instruction(s)", "", patch_range)
PluginCommand.register_for_address("VIBES\\Insert patch at this address", "", patch_addr)
