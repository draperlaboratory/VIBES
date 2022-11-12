from binaryninja import *

from binaryninjaui import (
  Menu,
  View,
  ViewType,
  ViewFrame,
  UIContext,
  UIAction,
  UIActionHandler,
  HexEditor,
  getMonospaceFont,
  ContextMenuManager,
)

import binaryninjaui

from binaryninjaui import (
  getMonospaceFont,
)

from PySide6.QtWidgets import (
  QHBoxLayout,
  QPlainTextEdit,
  QScrollArea,
  QListWidget,
  QTabWidget,
  QLabel,
  QSplitter,
  QMessageBox,
  QDialog,
  QVBoxLayout,
  QWidget,
  QPushButton,
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QPalette

def textedit():
  edit = QPlainTextEdit()
  edit.setFont(getMonospaceFont(edit))
  edit.setTabStopDistance(20)
  return edit

class Tab():
  def __init__(self, widget, name):
    self.widget = widget
    self.name = name

class Tabs():
  def __init__(self, tabs, idx):
    self.tabs = tabs
    self.idx = idx

class PatchEditor(QDialog):
  def __init__(self, context, parent=None):
    super(PatchEditor, self).__init__(parent)
    # Self
    self.title = QLabel(self.tr("VIBES Patch Editor"))
    self.tabs = {}
    # The view of the currently selected patch
    self.patch_view = QTabWidget()
    # The list of available patches
    self.patch_list = QListWidget()
    self.patch_list.currentItemChanged.connect(self._select_patch)
    self.patch_list.addItem("test1")
    self.patch_list.addItem("test2")
    self.patch_list.addItem("test3")
    # Patches meta-widget
    self.delete_button = QPushButton(self.tr("&Delete"))
    self.delete_button.clicked.connect(self._delete_patch)
    self.patches_buttons = QHBoxLayout()
    self.patches_buttons.addWidget(self.delete_button)
    self.patches = QVBoxLayout()
    self.patches.addWidget(self.patch_list)
    self.patches.addLayout(self.patches_buttons)
    self.patches_widget = QWidget()
    self.patches_widget.setLayout(self.patches)
    # Splitter between the list of patches and the current patch
    self.splitter = QSplitter()
    self.splitter.addWidget(self.patch_view)
    self.splitter.addWidget(self.patches_widget)
    # Layout
    self.hlayout = QHBoxLayout()
    self.hlayout.addWidget(self.splitter)
    self.showNormal()
    self.setLayout(self.hlayout)

  def _select_patch(self, current, previous):
    if previous:
      name = previous.text()
      if name in self.tabs:
        self.tabs[name].idx = self.patch_view.currentIndex()
    self.patch_view.clear()
    if current:
      name = current.text()
      if name not in self.tabs:
        tabs = [
          Tab(QWidget(), "Info"),
          Tab(textedit(), "C Code")
        ]
        self.tabs[name] = Tabs(tabs, 0)
      x = self.tabs[name]
      for t in x.tabs:
        self.patch_view.addTab(t.widget, t.name)
      self.patch_view.setCurrentIndex(x.idx)
      
  def _delete_patch(self):
    items = self.patch_list.selectedItems()
    if not items:
      return
    for item in items:
      del self.tabs[item.text()]
      self.patch_list.takeItem(self.patch_list.row(item))
    
 
PATCH_EDITOR = 'VIBES\\Patch Editor'
patch_editor = None

# functions
def launch_plugin(context):
  global patch_editor
  if not patch_editor:
    patch_editor = PatchEditor(context, parent=context.widget)
  patch_editor.show()


UIAction.registerAction(PATCH_EDITOR)
UIActionHandler.globalActions().bindAction(PATCH_EDITOR, UIAction(launch_plugin))
Menu.mainMenu('Plugins').addAction(PATCH_EDITOR, 'show')
