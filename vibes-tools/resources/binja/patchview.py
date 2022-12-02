import re

from . import db
from . import utils
from .vars import (HigherVar, PatchVar)
from .patchinfo import PatchInfo
from binaryninja import *
from binaryninjaui import getMonospaceFont

from PySide6.QtWidgets import (
  QFormLayout,
  QHBoxLayout,
  QHeaderView,
  QLineEdit,
  QPlainTextEdit,
  QLabel,
  QTreeWidget,
  QTreeWidgetItem,
  QVBoxLayout,
  QWidget,
  QPushButton,
  QComboBox,
  QHeaderView,
)

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
      utils.eprint("A variable with the name '%s' already exists" % name)
    elif not valid_var_name(name):
      utils.eprint("Invalid variable name '%s'" % name)
    else:
      return name


# Container class for the GUI elements for editing a patch.
class PatchView:
  def __init__(self, name, bv, addr, size, parent=None):
    self.idx = 0
    self.name = name
    self.bv = bv

    patches = db.get_patches(bv)
    if name not in patches:
      p = PatchInfo(name, addr, size, bv)
      patches[name] = p
      db.save_patch(bv, p)
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
    self.patch_vars_widget.header().setSectionResizeMode(QHeaderView.Stretch)
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
    self.higher_vars_widget.header().setSectionResizeMode(QHeaderView.Stretch)
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

    self.code_edit = \
      QPlainTextEdit(db.get_patch_code(bv, name), self.code_widget)
    self.code_edit.setFont(getMonospaceFont(self.code_edit))
    self.code_edit.setTabStopDistance(20)
    code_widget_layout.addWidget(self.code_edit)

    code_edit_buttons = QWidget(self.code_widget)
    code_edit_buttons_layout = QHBoxLayout()

    code_edit_save = QPushButton("Save to database", code_edit_buttons)
    code_edit_save.clicked.connect(self._save_code)
    code_edit_buttons_layout.addWidget(code_edit_save)
    code_edit_load = QPushButton("Load C source", code_edit_buttons)
    code_edit_load.clicked.connect(self._load_code)
    code_edit_buttons_layout.addWidget(code_edit_load)
    code_edit_buttons.setLayout(code_edit_buttons_layout)
    code_widget_layout.addWidget(code_edit_buttons)

    self.code_widget.setLayout(code_widget_layout)

  def add_to_widget(self, tab_widget):
    tab_widget.addTab(self.info_widget, "Info")
    tab_widget.addTab(self.code_widget, "C Code")
    tab_widget.setCurrentIndex(self.idx)

  def c_code(self):
    return self.code_edit.document().toPlainText()

  def _save_code(self):
    db.save_patch_code(self.bv, self.name, self.c_code())

  def _load_code(self):
    filename = get_open_filename_input("filename:", "*.c")
    if filename is None:
      return
    with open(filename) as f:
      self.code_edit.document().setPlainText(f.read())

  def current_patch(self):
    patches = db.get_patches(self.bv)
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
      db.save_patch(self.bv, p)
    except Exception:
      utils.eprint("Invalid patch point: " + text)
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
      db.save_patch(self.bv, p)
    except Exception:
      utils.eprint("Invalid patch size: " + text)
      p.size = old
      self.patch_size_widget.setText("%d" % old)

  def _update_sp_align(self):
    p = self.current_patch()
    old = p.sp_align
    text = self.sp_align_widget.text()
    try:
      p.sp_align = int(text)
      self.sp_align_widget.setText("%d" % p.sp_align)
      db.save_patch(self.bv, p)
    except Exception:
      utils.eprint("Invalid SP adjustment: " + text)
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
    db.save_patch(self.bv, p)

  def _add_var_impl(self, name, value, type):
    var = QTreeWidgetItem(self.patch_vars_widget)
    var.setText(0, name)
    type_combo = QComboBox(self.patch_vars_widget)
    type_combo.addItem("Register", HigherVar.REG_VAR)
    type_combo.addItem("Frame", HigherVar.FRAME_VAR)
    type_combo.addItem("Global", HigherVar.GLOBAL_VAR)
    type_combo.addItem("Function", HigherVar.FUNCTION_VAR)
    type_combo.addItem("Constant", HigherVar.CONSTANT_VAR)
    type_combo.setCurrentIndex(type)
    type_combo.currentIndexChanged.connect(self._var_type_changed)
    self._add_var_type(var, value, type)
    self.patch_vars_widget.setItemWidget(var, 1, type_combo)
    self.patch_vars_widget.addTopLevelItem(var)

  def _add_higher_var(self):
    item = self.higher_vars_widget.currentItem()
    var = item.parent()
    if var is None:
      utils.eprint("No higher var value is selected")
      return
    name = var.text(0)
    p = self.current_patch()
    if name in p.vars:
      utils.eprint("Variable %s already exists in the patch variables" % name)
      return
    hvar = self.hvars[name][var.indexOfChild(item)]
    self._add_var(p, name, hvar=hvar)

  def _add_var_type(self, var, value, type):
    name = var.text(0)
    if type == HigherVar.REG_VAR:
      regs = utils.available_regs(self.bv)
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
      regs = utils.available_regs(self.bv, include_sp=True)
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
    elif type == HigherVar.CONSTANT_VAR:
      value_widget = QLineEdit(("0x%x" % value[0]), self.patch_vars_widget)
      value_widget.editingFinished.connect(self._hex_changed)
      if value[1] == 1:
        i = 0
      elif value[1] == 2:
        i = 1
      elif value[1] == 4:
        i = 2
      elif value[1] == 8:
        i = 3
      else:
        assert False
      size_combo = QComboBox(self.patch_vars_widget)
      size_combo.addItems(["1", "2", "4", "8"])
      size_combo.setCurrentIndex(i)
      size_combo.currentIndexChanged.connect(self._const_size_changed)
      self.hex_var_widget[name] = value_widget
      value_ = QTreeWidgetItem(var)
      value_.setText(0, "Value")
      size = QTreeWidgetItem(var)
      size.setText(0, "Size (in bytes)")
      var.addChild(value_)
      var.addChild(size)
      self.patch_vars_widget.setItemWidget(value_, 1, value_widget)
      self.patch_vars_widget.setItemWidget(size, 1, size_combo)
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
    db.save_patch(self.bv, p)

  def _var_type_changed(self, type):
    var = self._current_var_item()
    if var is None:
      return
    var.takeChildren()
    p = self.current_patch()
    if type == HigherVar.REG_VAR:
      value = None
    elif type == HigherVar.FRAME_VAR:
      value = (utils.available_regs(self.bv, include_sp=True)[0], 0)
    elif type == HigherVar.GLOBAL_VAR or \
         type == HigherVar.FUNCTION_VAR:
      value = 0
    elif type == HigherVar.CONSTANT_VAR:
      value = 0, self.bv.arch.address_size
    else:
      assert False
    name = var.text(0)
    if name in self.hex_var_widget:
      del self.hex_var_widget[name]
    p.set_var(var.text(0), value, type)
    self._add_var_type(var, value, type)
    db.save_patch(self.bv, p)

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
    reg = None if i == 0 else utils.available_regs(self.bv)[i - 1]
    p = self.current_patch()
    p.vars[var.text(0)].set_at_entry(reg)
    db.save_patch(self.bv, p)

  def _at_exit_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    reg = None if i == 0 else utils.available_regs(self.bv)[i - 1]
    p = self.current_patch()
    p.vars[var.text(0)].set_at_exit(reg)
    db.save_patch(self.bv, p)

  def _base_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    reg = utils.available_regs(self.bv, include_sp=True)[i]
    p = self.current_patch
    v = self.current_patch().vars[var.text(0)]
    v.value = (reg, v.value[1])
    db.save_patch(self.bv, p)

  def _hex_changed(self):
    var = self._current_var_item()
    if var is None:
      return
    name = var.text(0)
    p = self.current_patch()
    v = p.vars[name]
    if v.type == HigherVar.FRAME_VAR:
      old = v.value[1]
    elif v.type == HigherVar.FUNCTION_VAR:
      old = v.value
    elif v.type == HigherVar.CONSTANT_VAR:
      old = v.value[0]
    else:
      assert False
    widget = self.hex_var_widget[name]
    text = widget.text()
    try:
      new = int(text, base=16)
      if v.type == HigherVar.FRAME_VAR:
        v.value = (v.value[0], new)
      elif v.type == HigherVar.FUNCTION_VAR:
        v.value = new
      elif v.type == HigherVar.CONSTANT_VAR:
        maximum = (1 << (v.value[1] * 8)) - 1
        new = maximum if new > maximum else new
        v.value = (new, v.value[1])
      else:
        assert False
      widget.setText("0x%x" % new)
      db.save_patch(self.bv, p)
    except Exception:
      utils.eprint("Invalid hex value: " + text)
      widget.setText("0x%x" % old)

  def _const_size_changed(self, i):
    var = self._current_var_item()
    if var is None:
      return
    p = self.current_patch()
    v = p.vars[var.text(0)]
    val, _ = v.value
    v.value = (val, 1 << i)
    db.save_patch(self.bv, p)
