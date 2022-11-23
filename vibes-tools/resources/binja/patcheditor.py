from . import db
from . import ogre
from .patchview import PatchView

from PySide6.QtWidgets import (
  QHBoxLayout,
  QListWidget,
  QTabWidget,
  QSplitter,
  QDialog,
  QVBoxLayout,
  QWidget,
  QPushButton,
)


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
    patches = db.get_patches(self.data)
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

    self.ogre = ogre.OGREEditor(context, self.container)

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
      patches = db.get_patches(self.data)
      name = item.text()
      del patches[name]
      del self.patches[name]
      db.delete_patch(self.data, name)
      self.patch_list_widget.takeItem(self.patch_list_widget.row(item))
