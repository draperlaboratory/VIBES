from . import db
from . import utils
from binaryninja import *

from PySide6.QtWidgets import (
  QHBoxLayout,
  QHeaderView,
  QTableWidget,
  QTableWidgetItem,
  QVBoxLayout,
  QWidget,
  QPushButton,
  QHeaderView,
)


class PatchSpaces:
  def __init__(self):
    self.spaces = []

  def clear(self):
    self.spaces.clear()
    
  def add(self, space: AddressRange):
    self.spaces.append(space)

  def remove(self, space: AddressRange):
    try:
      self.spaces.remove(space)
    except ValueError:
      pass

  def serialize(self, bv):
    result = []
    for space in self.spaces:
      result.append(PatchSpaces._serialize_space(space, bv))
    return result

  @staticmethod
  def _serialize_space(space, bv):
    return {
      "address": "0x%x:%d" % (space.start, bv.arch.address_size),
      "size": space.end - space.start
    }

  @staticmethod
  def deserialize(spaces):
    result = PatchSpaces()
    for d in spaces:
      result.add(PatchSpaces._deserialize_space(d))
    return result

  @staticmethod
  def _deserialize_space(d):
    start = int(d["address"].split(":")[0], base=16)
    size = d["size"]
    return AddressRange(start, end=start+size)


class PatchSpacesEditor(QWidget):
  def __init__(self, context, parent=None):
    super(PatchSpacesEditor, self).__init__(parent)
    
    self.data = context.binaryView
    self.currentOffset = 0

    self.container = QWidget(parent)

    self.spaces = PatchSpaces()
    
    self.spaces_widget = QTableWidget(self.container)
    self.spaces_widget.setColumnCount(2)
    self.spaces_widget.setHorizontalHeaderLabels(["Address", "Size"])
    self.spaces_widget.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.spaces_widget.itemChanged.connect(self._item_changed)

    add_space_button = QPushButton("Add", self.container)
    add_space_button.clicked.connect(self._new_space)
    remove_space_button = QPushButton("Remove", self.container)
    remove_space_button.clicked.connect(self._remove_current_space)

    button_layout = QHBoxLayout()
    button_layout.addWidget(add_space_button)
    button_layout.addWidget(remove_space_button)
    
    button_widget = QWidget(self.container)
    button_widget.setLayout(button_layout)

    layout = QVBoxLayout()
    layout.addWidget(self.spaces_widget)
    layout.addWidget(button_widget)
    self.setLayout(layout)

  def add_space(self, addr, n):
    c = self.spaces_widget.rowCount()
    self.spaces_widget.insertRow(c)
    self.spaces_widget.setItem(c, 0, QTableWidgetItem("0x%x" % addr))
    self.spaces_widget.setItem(c, 1, QTableWidgetItem("%d" % n))
    self._reset_spaces()

  def _new_space(self):
    self.add_space(0, 0)

  def _reset_spaces(self):
    self.spaces.clear()
    for row in range(self.spaces_widget.rowCount()):
      space = self._deserialize_row(row)
      if space is not None:
        self.spaces.add(space)
      
  def _item_changed(self, item):
    try:
      self._reset_spaces()
    except ValueError:
      if item.column() == 0:
        utils.eprint("Address %s is ill-formed (must be a hex integer)" % item.text())
      else:
        utils.eprint("Size %s is ill-formed (must be a decimal integer)" % item.text())
      self.spaces_widget.removeRow(item.row())

  def _deserialize_row(self, row):
    start_item = self.spaces_widget.item(row, 0)
    if start_item is None:
      return None
    size_item = self.spaces_widget.item(row, 1)
    if size_item is None:
      return None
    start = int(start_item.text(), base=16)
    size = int(size_item.text())
    return AddressRange(start, end=start+size)
      
  def _remove_current_space(self):
    row = self.spaces_widget.currentRow()
    if row == -1:
      return
    self._remove_space(row)

  def _remove_space(self, row):
    self.spaces_widget.removeRow(row)
    try:
      space = self._deserialize_row(row)
      if space is None:
        return
      self.spaces.remove(space)
    except ValueError:
      pass
