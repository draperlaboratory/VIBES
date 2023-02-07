# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

import os
import json
import subprocess

from binaryninja.interaction import (
  get_directory_name_input,
  get_open_filename_input,
)

from . import db
from . import ogre
from . import patchspaces
from . import utils
from .patchview import PatchView
from .patchinfo import PatchInfo

from PySide6.QtGui import QAction

from PySide6.QtWidgets import (
  QHBoxLayout,
  QListWidget,
  QMenuBar,
  QTabWidget,
  QSplitter,
  QDialog,
  QVBoxLayout,
  QWidget,
  QPushButton,
  QMenuBar,
  QMessageBox,
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

    if self.patch_list_widget.count():
      self.patch_list_widget.setCurrentRow(0)

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
    self.spaces = patchspaces.PatchSpacesEditor(context, self.container)

    tabs = QTabWidget(self.container)
    tabs.addTab(splitter, "Patches")
    tabs.addTab(self.ogre, "OGRE")
    tabs.addTab(self.spaces, "Patch Spaces")

    menu_bar = QMenuBar(self.container)

    file_menu = menu_bar.addMenu("File")
    file_import_act = QAction("Import config", file_menu)
    file_import_act.setStatusTip("Imports a VIBES patch configuration file.")
    file_import_act.triggered.connect(self._import)
    file_export_act = QAction("Export", file_menu)
    file_export_act.setStatusTip("Exports the patch information to the VIBES input format.")
    file_export_act.triggered.connect(self._export)
    file_menu.addAction(file_import_act)
    file_menu.addAction(file_export_act)

    help_menu = menu_bar.addMenu("Help")
    help_about_act = QAction("About", help_menu)
    help_about_act.triggered.connect(self._about)
    help_menu.addAction(help_about_act)

    layout = QVBoxLayout()
    layout.addWidget(menu_bar)
    layout.addWidget(tabs)

    self.setLayout(layout)

  def add_patch(self, name, addr, size):
    if name not in self.patches:
      self.patches[name] = PatchView(name, self.data, addr, size)
      self.patch_list_widget.addItem(name)
    self.current_patch = self.patches[name]
    self.current_patch.add_to_widget(self.patch_tab_widget)

  def _add_existing_patch(self, p):
    db.save_patch(self.data, p)
    self.add_patch(p.name, p.addr, p.size)

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
      row = self.patch_list_widget.row(item)
      self.patch_list_widget.takeItem(row)

  def _import(self):
    filename = \
      get_open_filename_input("Choose a VIBES configuration file",
                              "*.info.json")
    if filename is None:
      return

    with open(filename) as f:
      name = os.path.basename(filename)[:-10]
      if name in self.patches:
        utils.eprint("Patch %s already exists" % name)
        return
      j = json.loads(f.read())
      p = PatchInfo.deserialize(self.data, name, j)
      self._add_existing_patch(p)

  def _export(self):
    savedir = get_directory_name_input("Choose a directory to export to")
    if savedir is None:
      return

    def filename(name, ext):
      return "%s/%s.%s" % (savedir, name, ext)

    patches = db.get_patches(self.data)
    for name, pview in self.patches.items():
      p = patches[name]
      c = pview.c_code()
      info = json.dumps(p.serialize(self.data), indent=4)
      with open(filename(name, "info.json"), "w") as f:
        f.write(info)
      with open(filename(name, "c"), "w") as f:
        f.write(c)

    with open(filename("patch-spaces", "json"), "w") as f:
      spaces = db.get_spaces(self.data)
      s = patchspaces.serialize(self.data, spaces)
      j = json.dumps(s, indent=4)
      f.write(j)

    names = []
    for name in self.patches.keys():
      names.append(name)

    args = [
      "vibes-init",
      "--binary=%s" % self.data.file.original_filename,
      "--patched-binary=patched.exe",
      "--patch-names=%s" % ",".join(names),
    ]

    # vibes-init doesn't always correctly infer whether the binary is
    # using the Thumb encoding or not.
    if self.data.arch.name.startswith("thumb2"):
      args.append("--language=bap:llvm-thumb")

    ogre = self.ogre.ogre
    if ogre.functions:
      name = filename("loader", "ogre")
      with open(name, "w") as f:
        f.write(str(ogre))
        args.append("--ogre=%s" % name)

    print("Running vibes-init in", savedir)
    proc = subprocess.run(args, cwd=savedir, stderr=subprocess.PIPE)
    print("vibes-init exited with code", proc.returncode)
    if proc.returncode != 0:
      utils.eprint(proc.stderr.decode())

  def _about(self):
    text = """
    This is the Binary Ninja front-end plugin for VIBES.

    VIBES (Verified, Incremental Binary Editing with
    Synthesis) is a compiler for binary micro-patching,
    developed by the Formal Methods group at The Charles
    Stark Draper Laboratory.

    This work is sponsored by DARPA / NAVWAR Contract
    N6600120C4018, as part of the DARPA Assured
    Micro-Patching (AMP) program. Its content does not
    necessarily reflect the position or policy of the
    US Government and no official endorsement should be
    inferred.
    """

    mbox = QMessageBox()
    mbox.setIcon(QMessageBox.Information)
    mbox.setText(text)
    mbox.setWindowTitle("About VIBES")
    mbox.setStandardButtons(QMessageBox.Ok)
    mbox.exec()
