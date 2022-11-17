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
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QPalette

def eprint(*args, **kwargs):
  print(*args, file=sys.stderr, **kwargs)

patches = {}


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
      assert(False)

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
      assert(False)


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
    assert(False)


# Metadata for a patch.
class Patch:
  def __init__(self, name, addr, size, bv):
    self.name = name
    self.addr = addr
    self.size = size
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

  def collect_higher_vars(self, bv):
    end = self.end_addr()
    f = self.function(bv)
    l = f.get_low_level_il_at(end)
    result = {}

    def add(name, h):
      if name not in result:
        result[name] = [h]
      else:
        result[name].append(h)

    def defined_at_patch(v):
      for d in f.hlil.get_var_definitions(v):
        if d.address >= self.addr or d.address <= end:
          return True
      return False

    vars = f.hlil.vars
    i = l.mlil.hlil.instr_index
    for v in vars:
      # Disregard this variable if it was defined within
      # the patch region (or at the very end).
      if defined_at_patch(v):
        continue

      if f.hlil.is_var_live_at(v, i):
        if v.source_type == VariableSourceType.RegisterVariableSourceType:
          r = bv.arch.get_reg_name(v.storage).upper()
          add(v.name, HigherVar(v.name, r, HigherVar.REG_VAR))
        elif v.source_type == VariableSourceType.StackVariableSourceType:
          sp = bv.arch.stack_pointer
          sp_val = l.get_reg_value(sp)
          off = -self.sp_align
          if sp_val.type == RegisterValueType.StackFrameOffset:
            off += sp_val.value
          value = (sp.upper(), v.storage - off)
          add(v.name, HigherVar(v.name, value, HigherVar.FRAME_VAR))

    regs = f.llil.regs
    for r in regs:
      v = l.get_reg_value(r)
      if v.type == RegisterValueType.ConstantPointerValue:
        s = bv.get_symbol_at(v.value)
        if s:
          add(s.name, HigherVar(s.name, r.name.upper(), HigherVar.REG_VAR))

    for s in bv.get_symbols():
      if s.auto:
        continue
      if s.type == SymbolType.DataSymbol:
        add(s.name, HigherVar(s.name, s.address, HigherVar.GLOBAL_VAR))
      elif s.type == SymbolType.FunctionSymbol:
        add(s.name, HigherVar(s.name, s.address, HigherVar.FUNCTION_VAR))

    return result


# Container class for the GUI elements for editing a patch.
class PatchView:
  def __init__(self, name, bv, addr, size, parent=None):
    self.idx = 0
    self.name = name
    self.bv = bv

    global patches
    if name not in patches:
      patches[name] = Patch(name, addr, size, bv)
    p = patches[name]

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

    self.higher_vars_widget = QTreeWidget(self.info_widget)
    self.higher_vars_widget.setColumnCount(2)
    self.higher_vars_widget.setHeaderLabels(["Variable", "Storage"])
    self._update_higher_vars()
    info_layout.addRow("Higher variables", self.higher_vars_widget)

    self.info_widget.setLayout(info_layout)

    self.code_widget = QPlainTextEdit(parent)
    self.code_widget.setFont(getMonospaceFont(self.code_widget))
    self.code_widget.setTabStopDistance(20)

  def add_to_widget(self, tab_widget):
    tab_widget.addTab(self.info_widget, "Info")
    tab_widget.addTab(self.code_widget, "C Code")
    tab_widget.setCurrentIndex(self.idx)

  def c_code(self):
    return self.code_widget.document().toPlainText()

  def _update_higher_vars(self):
    global patches
    self.higher_vars_widget.clear()
    hvars = patches[self.name].collect_higher_vars(self.bv)
    for name, vs in hvars.items():
      var = QTreeWidgetItem(self.higher_vars_widget)
      var.setText(0, name)
      for h in vs:
        storage = QTreeWidgetItem(var)
        storage.setText(0, h.type_str())
        storage.setText(1, h.value_str())
        var.addChild(storage)
      self.higher_vars_widget.addTopLevelItem(var)

  def _update_patch_point(self):
    global patches
    p = patches[self.name]
    old = p.addr
    text = self.patch_point_widget.text()
    try:
      p.addr = int(text, base=16)
      f = p.function(self.bv)
      self.function_label.setText(f.name)
      self._update_higher_vars()
    except Exception:
      eprint("Invalid patch point: " + text)
      p.addr = old
      self.patch_point_widget.setText("0x%x" % old)

  def _update_patch_size(self):
    global patches
    p = patches[self.name]
    old = p.size
    text = self.patch_size_widget.text()
    try:
      p.size = int(text)
      assert(p.size >= 0)
      self._update_higher_vars()
    except Exception:
      eprint("Invalid patch size: " + text)
      p.size = old
      self.patch_size_widget.setText("%d" % old)

  def _update_sp_align(self):
    global patches
    p = patches[self.name]
    old = p.sp_align
    text = self.sp_align_widget.text()
    try:
      p.sp_align = int(text)
      self._update_higher_vars()
    except Exception:
      eprint("Invalid SP adjustment: " + text)
      p.sp_align = old
      self.sp_align_widget.setText("%d" % old)


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
    layout = QHBoxLayout()

    self.patch_tab_widget = QTabWidget(self.container)

    self.patch_list_widget = QListWidget(self.container)
    self.patch_list_widget.currentItemChanged.connect(self._select_patch)

    # We may have started adding patches before we constructed this
    # dialog object, so we should synchronize this data.
    global patches
    for name, p in patches.items():
      if name not in self.patches:
        self.add_patch(name, p.addr, p.size)

    self.patch_delete_button = QPushButton(self.tr("Delete"))
    self.patch_delete_button.clicked.connect(self._delete_patch)

    patch_buttons_layout = QHBoxLayout()
    patch_buttons_layout.addWidget(self.patch_delete_button)

    patches_layout = QVBoxLayout()
    patches_layout.addWidget(self.patch_list_widget)
    patches_layout.addLayout(patch_buttons_layout)

    patches_widget = QGroupBox("Patches", self.container)
    patches_widget.setLayout(patches_layout)

    splitter = QSplitter(self.container)
    splitter.addWidget(patches_widget)
    splitter.addWidget(self.patch_tab_widget)
    splitter.setChildrenCollapsible(False)

    # Make the list widget smaller
    w = max(patches_widget.minimumSizeHint().width(),
            self.patch_tab_widget.minimumSizeHint().width())
    splitter.setSizes([int(w / 2), w + int(w / 2)])

    layout = QVBoxLayout()
    layout.addWidget(splitter)
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
      global patches
      name = item.text()
      del patches[name]
      del self.patches[name]
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

def launch_plugin(context):
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
UIActionHandler.globalActions().bindAction(PATCH_EDITOR, UIAction(launch_plugin))
Menu.mainMenu('Plugins').addAction(PATCH_EDITOR, 'show')

def prompt_patch_name():
  global patches
  while True:
    name = get_text_line_input("Please provide a patch name",
                               "VIBES: new patch").decode('ASCII')
    if name not in patches:
      return name
    eprint("A patch with the name '%s' already exists" % name)

def patch_range(bv, addr, n):
  if not check_arch(bv):
    return
  global patch_editor
  name = prompt_patch_name()
  if patch_editor:
    patch_editor.add_patch(name, addr, n)
  else:
    # Patch editor hasn't been created yet
    global patches
    patches[name] = Patch(name, addr, n, bv)

def patch_addr(bv, addr):
  if not check_arch(bv):
    return
  global patch_editor
  name = prompt_patch_name()
  if patch_editor:
    patch_editor.add_patch(name, addr, 0)
  else:
    # Patch editor hasn't been created yet
    global patches
    patches[name] = Patch(name, addr, 0, bv)

PluginCommand.register_for_range("VIBES\\Patch highlighted instruction(s)", "", patch_range)
PluginCommand.register_for_address("VIBES\\Insert patch at this address", "", patch_addr)
