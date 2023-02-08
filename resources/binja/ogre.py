# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

from . import db
from . import utils
from binaryninja import *

from PySide6.QtWidgets import (
  QHBoxLayout,
  QGroupBox,
  QListWidget,
  QVBoxLayout,
  QWidget,
  QPushButton,
)

OGRE_DECLS = \
  """(declare arch (name str))
(declare bits (size int))
(declare base-address (addr int))
(declare entry-point (addr int))
(declare is-little-endian (flag bool))
(declare mapped (addr int) (size int) (off int))
(declare code-region (addr int) (size int) (off int))
(declare named-region (addr int) (size int) (name str))
(declare segment (addr int) (size int) (r bool) (w bool) (x bool))
(declare section (addr int) (size int))
(declare code-start (addr int))
(declare named-symbol (addr int) (name str))
(declare symbol-chunk (addr int) (size int) (root int))
(declare symbol-value (addr int) (value int))"""


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
  def __init__(self, addr, size, r, w, x):
    self.addr = addr
    self.size = size
    self.r = r
    self.w = w
    self.x = x

  def __str__(self):
    r = "true" if self.r else "false"
    w = "true" if self.w else "false"
    x = "true" if self.x else "false"
    return "(segment (addr 0x%x) (size 0x%x) (r %s) (w %s) (x %s))" % \
      (self.addr, self.size, r, w, x)


class OGRESymbolChunk:
  def __init__(self, addr, size, root):
    self.addr = addr
    self.size = size
    self.root = root

  def __str__(self):
    return "(symbol-chunk (addr 0x%x) (size 0x%x) (root 0x%x))" % \
      (self.addr, self.size, self.root)


class OGRESymbolValue:
  def __init__(self, addr, value):
    self.addr = addr
    self.value = value

  def __str__(self):
    return "(symbol-value (addr 0x%x) (value 0x%x))" % \
      (self.addr, self.value)


class OGREFunction:
  def __init__(self, bv, f):
    self.name = f.name
    self.addr = f.start
    self.off = utils.addr_to_off(bv, f.start)
    self.mapped = []
    self.code_regions = []
    self.named_regions = []
    self.symbol_chunks = []
    value = f.start
    if f.arch.name.startswith("thumb2"):
      value |= 1
    self.symbol_value = OGRESymbolValue(f.start, value)
    for r in f.address_ranges:
      name = "%s@%x" % (f.name, r.start)
      size = r.end - r.start
      off = utils.addr_to_off(bv, r.start)
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
      "\n".join(map(lambda x: str(x), self.symbol_chunks)),
      str(self.symbol_value)
    ])


class OGREData(OGREAddrSizeOff):
  def __init__(self, addr, size, off, writable, name):
    super(OGREData, self).__init__("", addr, size, off)
    self.name = name
    self.writable = writable
    self.refs = 1

  def __str__(self):
    mapped = OGREMapped(self.addr, self.size, self.off)
    segment = OGRESegment(self.addr, self.size,
                          r=True, w=self.writable, x=False)
    region = OGRENamedRegion(self.addr, self.size, self.name)
    return "\n".join([str(mapped), str(segment), str(region)])


class OGRE:
  def __init__(self, bv):
    self.bv = bv
    is_thumb = bv.arch.name.startswith("thumb2")
    if is_thumb or bv.arch.name.startswith("armv7"):
      self.arch = "arm"
    else:
      assert False
    self.bits = bv.address_size * 8
    self.base_address = bv.start
    self.is_little_endian = bv.endianness == Endianness.LittleEndian
    self.entry_point = bv.entry_point
    if is_thumb:
      self.entry_point = self.entry_point & ~1
    self.functions = {}
    self.rodata = {}

  def add_function(self, f):
    if f.start in self.functions:
      return False
    self.functions[f.start] = OGREFunction(self.bv, f)
    for r, d in utils.rodata_of_func(self.bv, f).items():
      if r in self.rodata:
        self.rodata[r].refs += 1
      else:
        self.rodata[r] = \
          OGREData(addr=r, size=d[0], off=d[1], writable=False, name=d[2])
    return True

  def delete_function(self, f):
    remove = set()
    rodata = utils.rodata_of_func(self.bv, f)
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
    self.currentOffset = 0

    self.container = QWidget(parent)

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

    update_funcs_button = QPushButton("Update available functions", self.container)
    update_funcs_button.clicked.connect(self._update_functions)

    for name in db.get_ogre_functions(self.data):
      if not self._add_function_by_name(name, save=False):
        db.remove_ogre_func(self.data, name)

    layout = QVBoxLayout()
    layout.addWidget(functions_widget)
    layout.addWidget(update_funcs_button)
    self.setLayout(layout)

  def _update_functions(self):
    names = []
    self.available_funcs_widget.clear()
    self.functions.clear()
    for s in self.data.get_symbols():
      if s.auto:
        if s.type != SymbolType.ImportedFunctionSymbol:
          continue
      elif s.type != SymbolType.FunctionSymbol:
        continue
      f = self.data.get_function_at(s.address)
      name = f.name
      if name in self.functions:
        name = "%s@%x" % (name, f.start)
      self.functions[name] = f
      names.append(name)
    names.sort()
    for name in names:
      self.available_funcs_widget.addItem(name)
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
        db.save_ogre_func(self.data, name)
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
    db.remove_ogre_func(self.data, name)
