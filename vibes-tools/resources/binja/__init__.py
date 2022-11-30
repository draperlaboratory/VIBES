import re

from . import db
from . import utils
from .patchinfo import PatchInfo
from .patcheditor import PatchEditor
from binaryninja import *

from binaryninjaui import (
  Menu,
  UIAction,
  UIActionHandler,
)

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

def check_bv_changed(bv):
  global patch_editor
  if patch_editor is None:
    return
  if patch_editor.data.file.filename == bv.file.filename:
    return
  patch_editor = None
  db.clear_patches()

def launch_editor(context):
  global patch_editor
  bv = context.binaryView
  if not bv:
    show_message_box("VIBES", "No binary currently in view",
                     MessageBoxButtonSet.OKButtonSet,
                     MessageBoxIcon.ErrorIcon)
    return
  if not check_arch(bv):
    return
  check_bv_changed(bv)
  if not patch_editor:
    patch_editor = PatchEditor(context, parent=context.widget)
  patch_editor.show()

UIAction.registerAction(PATCH_EDITOR)
UIActionHandler.globalActions().bindAction(PATCH_EDITOR, UIAction(launch_editor))
Menu.mainMenu('Plugins').addAction(PATCH_EDITOR, 'show')

patch_name_re = re.compile("[A-Za-z][A-Za-z-_0-9]*")

def valid_patch_name(name):
 return patch_name_re.fullmatch(name) is not None

def prompt_patch_name(bv):
  patches = db.get_patches(bv)
  while True:
    name = get_text_line_input("Please provide a patch name",
                               "VIBES: new patch")
    if name is None:
      return None
    name = name.decode('ASCII')
    if name is None:
      return None
    elif name in patches:
      utils.eprint("A patch with the name '%s' already exists" % name)
    elif not valid_patch_name(name):
      utils.eprint("Invalid patch name '%s'" % name)
    else:
      return name

def patch_range(bv, addr, n):
  if not check_arch(bv):
    return
  check_bv_changed(bv)
  global patch_editor
  name = prompt_patch_name(bv)
  if name is None:
    return
  if patch_editor:
    patch_editor.add_patch(name, addr, n)
  else:
    # Patch editor hasn't been created yet
    p = PatchInfo(name, addr, n, bv)
    patches = db.get_patches(bv)
    patches[name] = p
    db.save_patch(bv, p)

def patch_addr(bv, addr):
  if not check_arch(bv):
    return
  check_bv_changed(bv)
  global patch_editor
  name = prompt_patch_name(bv)
  if name is None:
    return
  if patch_editor:
    patch_editor.add_patch(name, addr, 0)
  else:
    # Patch editor hasn't been created yet
    p = PatchInfo(name, addr, 0, bv)
    patches = db.get_patches(bv)
    patches[name] = p
    db.save_patch(bv, p)

def add_patch_space(bv, addr, n):
  if not check_arch(bv):
    return
  check_bv_changed(bv)
  global patch_editor
  if patch_editor:
    patch_editor.spaces.add_space(addr, n)
  else:
    # TODO
    pass

PluginCommand.register_for_range("VIBES\\Patch highlighted instruction(s)", "", patch_range)
PluginCommand.register_for_address("VIBES\\Insert patch at this address", "", patch_addr)
PluginCommand.register_for_range("VIBES\\Add patch space", "", add_patch_space)
