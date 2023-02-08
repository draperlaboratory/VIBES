# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

from . import utils
from .vars import (HigherVar, PatchVar)
from binaryninja import *

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
      if l is None:
        self.sp_align = 0
      else:
        sp = l.get_reg_value(bv.arch.stack_pointer)
        if sp.type == RegisterValueType.StackFrameOffset:
          a = utils.sp_alignment(bv)
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
  def deserialize(bv, name, d):
    addr = int(d["patch-point"].split(":")[0], base=16)
    size = d["patch-size"]
    p = PatchInfo(name, addr, size, None)
    p.sp_align = d["sp-align"]
    vars = map(lambda x: PatchVar.deserialize(bv, x), d["patch-vars"])
    for v in vars:
      p.vars[v.name] = v
    return p

  def collect_higher_vars(self, bv):
    end = self.end_addr()
    f = self.function(bv)

    lstart = f.get_low_level_il_at(self.addr)
    if lstart is None:
      utils.eprint("No LLIL instruction found at 0x%x" % self.addr)
      return {}

    lend = f.get_low_level_il_at(end)
    if lend is None:
      utils.eprint("No LLIL instruction found at 0x%x, using 0x%x" % (end, self.addr))
      lend = lstart

    mmlil = f.mapped_medium_level_il
    available_regs = utils.available_regs(bv)

    bb_patch_ll = f.llil.get_basic_block_at(lend.instr_index)
    bb_patch_ml = mmlil.get_basic_block_at(mmlil.get_instruction_start(end))
    result = {}

    def add(name, h):
      if name not in result:
        result[name] = [h]
      else:
        for h2 in result[name]:
          if h == h2:
            return
        result[name].append(h)

    def defined_after_patch(v, end):
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
      for u in mmlil.get_var_uses(v):
        bb_use = mmlil.get_basic_block_at(u.instr_index)
        if not reachable(bb_use, bb_patch_ml, u.address, self.addr):
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

    def is_ptr(type):
      return \
        isinstance(type, ArrayType) or \
        isinstance(type, PointerType)

    def handle_live_var(v):
      if v.source_type == VariableSourceType.RegisterVariableSourceType:
        s = spilled(v)
        if s is not None:
          frame = stack_frame(s)
          add(v.name, HigherVar(v.name, frame, HigherVar.FRAME_VAR))
        else:
          r = bv.arch.get_reg_name(v.storage).upper()
          if r in available_regs:
            add(v.name, HigherVar(v.name, r, HigherVar.REG_VAR))
      elif v.source_type == VariableSourceType.StackVariableSourceType:
        frame = stack_frame(v.storage)
        add(v.name, HigherVar(v.name, frame, HigherVar.FRAME_VAR))

    idxs = []
    if lend.mmlil.hlil:
      idxs.append((lend.mmlil.hlil.instr_index, end))
    else:
      # Sometimes there is no mapping all the way from LLIL to MLIL
      # to HLIL, but it can show up in the list of HLIL instructions
      # like so:
      def find(end):
        idx = None
        for insn in f.hlil.instructions:
          if insn.address == end:
            idx = insn.instr_index
            break
        return idx
      # Find the nearest set of blocks in the dominator tree where
      # we hit a complete mapping up to the HLIL.
      def traverse(bb, start=None):
        if start is None:
          start = bb.start
        for ii in range(start, bb.end):
          a = f.llil[ii].address
          i = find(a)
          if i is not None:
            idxs.append((i, a))
            return
        for d in bb.dominator_tree_children:
          traverse(d)
      # Start at the current block.
      start = lend.instr_index
      bb = f.llil.get_basic_block_at(start)
      traverse(bb, start)

    # Collect the live HLIL variables.
    for i, a in idxs:
      print("Exploring 0x%x, index %d" % (a, i))
      for v in f.hlil.vars:
        # Disregard this variable if it was defined within the patch
        # region, or at the very end.
        if defined_after_patch(v, a):
          continue
        # XXX: should we consider all variables that can be accessed
        # from this point, not just those that are live at the end
        # of the patch?
        if not f.hlil.is_var_live_at(v, i):
          if v in f.parameter_vars:
            handle_live_var(v)
        else:
          handle_live_var(v)

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
          if reg in available_regs and reg_live(r):
            if is_ptr(bv.get_data_var_at(v.value).type):
              name = s.name
            else:
              name = s.name + "__ptr"
            add(name, HigherVar(s.name, reg, HigherVar.REG_VAR))

    # XXX: how to deal with interworking here?
    def add_function(name, address):
      f = bv.get_function_at(address)
      if f is None or f.arch != bv.arch:
        return
      add(name, HigherVar(name, address, HigherVar.FUNCTION_VAR))

    # Grab all the known function and data symbols. We should
    # ignore those which were automatically named, to avoid
    # cluttering the output.
    flow, fhigh = utils.frame_range(bv)
    for s in bv.get_symbols():
      if s.auto:
        if s.type == SymbolType.ImportedFunctionSymbol:
          add_function(s.name, s.address)
      elif s.type == SymbolType.DataSymbol:
        a = s.address
        type = bv.get_data_var_at(a).type
        add(s.name, HigherVar(s.name, a, HigherVar.GLOBAL_VAR))
        # See if this symbol can be accessed via frame through a
        # register with a known value.
        for reg, val in possible_frames:
          off = a - val
          if (a < val and off >= flow)  or \
             (a > val and off <= fhigh) or \
             (off == 0 and not is_ptr(type)):
            add(s.name, HigherVar(s.name, (reg, off), HigherVar.FRAME_VAR))
      elif s.type == SymbolType.FunctionSymbol:
        add_function(s.name, s.address)

    return result
