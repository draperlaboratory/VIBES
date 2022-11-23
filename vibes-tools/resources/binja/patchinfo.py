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

    def is_ptr(type):
      return \
        isinstance(type, ArrayType)   or \
        isinstance(type, PointerType)

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
            if is_ptr(bv.get_data_var_at(v.value).type):
              name = s.name
            else:
              name = s.name + "__ptr"
            add(name, HigherVar(s.name, reg, HigherVar.REG_VAR))

    # Grab all the known function and data symbols. We should
    # ignore those which were automatically named, to avoid
    # cluttering the output.
    flow, fhigh = utils.frame_range(bv)
    for s in bv.get_symbols():
      if s.auto:
        continue
      if s.type == SymbolType.DataSymbol:
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
        add(s.name, HigherVar(s.name, s.address, HigherVar.FUNCTION_VAR))

    return result
