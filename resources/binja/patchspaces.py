# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

from PySide6.QtGui import QIntValidator
from . import db
from . import utils
from binaryninja import *

from PySide6.QtCore import QRegularExpression, QRegularExpressionMatchIterator

from PySide6.QtGui import (
  QIntValidator,
  QRegularExpressionValidator,
)

from PySide6.QtWidgets import (
  QHBoxLayout,
  QHeaderView,
  QLineEdit,
  QTreeWidget,
  QTreeWidgetItem,
  QVBoxLayout,
  QWidget,
  QPushButton,
  QHeaderView,
)

def serialize_space(space, bv):
  return {
    "address": "0x%x:%d" % (space.start, bv.arch.address_size * 8),
    "size": space.end - space.start
  }

def serialize(bv, spaces):
  result = []
  for space in spaces:
    result.append(serialize_space(space, bv))
  return result

def deserialize_space(d):
  start = int(d["address"].split(":")[0], base=16)
  size = d["size"]
  return AddressRange(start, end=start+size)

def deserialize(spaces):
  result = []
  for d in spaces:
    result.append(deserialize_space(d))
  return result


class PatchSpacesEditor(QWidget):
  def __init__(self, context, parent=None):
    super(PatchSpacesEditor, self).__init__(parent)
    
    self.data = context.binaryView
    self.currentOffset = 0

    self.container = QWidget(parent)

    self.spaces_widget = QTreeWidget(self.container)
    self.spaces_widget.setColumnCount(2)
    self.spaces_widget.setHeaderLabels(["Address", "Size"])
    self.spaces_widget.header().setSectionResizeMode(QHeaderView.Stretch)
    # We may have started adding patche spaces before we constructed this
    # dialog object, so we should synchronize this data.
    spaces = db.get_spaces(self.data)
    for space in spaces:
      self.add_space(space.start, space.end - space.start, reset=False)
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

  def add_space(self, addr, n, reset=True):
    item = QTreeWidgetItem(self.spaces_widget)
    addr_widget = QLineEdit(self.spaces_widget)
    size_widget = QLineEdit(self.spaces_widget)
    addr_widget.setText("0x%x" % addr)
    addr_limit = self.data.arch.address_size * 2
    addr_re = QRegularExpression("0x[0-9a-fA-F]{1,%d}" % addr_limit)
    addr_widget.setValidator(QRegularExpressionValidator(addr_re, addr_widget))
    size_widget.setText("%d" % n)
    size_widget.setValidator(QIntValidator(0, 0x1000000, size_widget))
    self.spaces_widget.setItemWidget(item, 0, addr_widget)
    self.spaces_widget.setItemWidget(item, 1, size_widget)
    if reset:
      self._reset_spaces()

  def _new_space(self):
    self.add_space(0, 0)

  def _reset_spaces(self):
    db.clear_spaces()
    for row in range(self.spaces_widget.topLevelItemCount()):
      space = self._deserialize_row(row)
      if space is not None:
        db.save_space(self.data, space)
      
  def _item_changed(self, _item, _column):
    self._reset_spaces()

  def _deserialize_row(self, row):
    item = self.spaces_widget.topLevelItem(row)
    if item is None:
      return None
    start_item = self.spaces_widget.itemWidget(item, 0)
    if not isinstance(start_item, QLineEdit):
      return None
    size_item = self.spaces_widget.itemWidget(item, 1)
    if not isinstance(size_item, QLineEdit):
      return None
    start = int(start_item.text(), base=16)
    size = int(size_item.text())
    return AddressRange(start, end=start+size)
      
  def _remove_current_space(self):
    item = self.spaces_widget.currentItem()
    if item is None:
      return
    while item.parent() is not None:
      item = item.parent()
    if item is None:
      return
    row = self.spaces_widget.indexOfTopLevelItem(item)
    self._remove_space(row)

  def _remove_space(self, row):
    space = self._deserialize_row(row)
    if space is None:
      return
    db.delete_space(self.data, space)
    self.spaces_widget.takeTopLevelItem(row)
