# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

import sys

from binaryninja import *

def eprint(*args, **kwargs):
  print(*args, file=sys.stderr, **kwargs)

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
  elif a == "ppc" or a == "ppc_le":
    return -32768, 32767
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

PPC_REGS = [
  "R0", "R2", "R3",
  "R4", "R5", "R6", "R7",
  "R8", "R9", "R10", "R11",
  "R12", "R13", "R14", "R15",
  "R16", "R17", "R18", "R19",
  "R20", "R21", "R22", "R23",
  "R24", "R25", "R26", "R27",
  "R28", "R29", "R30", "R31",
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
  elif a == "ppc" or a == "ppc_le":
    l = PPC_REGS.copy()
    if include_sp:
      l.insert(1, "R1")
    return l
  else:
    assert False

def addr_to_off_seg(seg, addr):
  return (addr - seg.start) + seg.data_offset

def addr_to_off(bv, addr):
  return addr_to_off_seg(bv.get_segment_at(addr), addr)
    
def rodata_of_func(bv, f):
  result = {}
  for l in f.llil.instructions:
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
      if d.name is None:
        name = "data_%x" % r
      else:
        name = d.name
      result[r] = (size, off, name)
  return result

def is_valid_sym_name(s):
  def not_auto(s, pre):
    if s.name:
      if s.auto and s.name.startswith(pre):
        return False
      else:
        return True
    else:
      return False
  if s.type != SymbolType.ImportedFunctionSymbol:
    if s.type == SymbolType.FunctionSymbol:
      return not_auto(s, "sub_") and not_auto(s, "j_sub_")
    elif s.type == SymbolType.DataSymbol:
      return not_auto(s, "data_")
  else:
    return True
