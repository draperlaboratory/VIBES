# Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.
#
# This file is provided under the license found in the LICENSE file in
# the top-level directory of this project.
#
# This research was developed with funding from the Defense Advanced
# Research Projects Agency (DARPA).

from binaryninja import AddressRange
from .patchinfo import PatchInfo

patches = {}

def clear_patches():
  global patches
  patches.clear()

spaces = []

def clear_spaces():
  global spaces
  spaces.clear()

def get_patches(bv):
  global patches
  try:
    ps = bv.query_metadata("vibes.patch-infos")
  except KeyError:
    ps = {}
    bv.store_metadata("vibes.patch-infos", ps)
  if not patches:
    for k, v in ps.items():
      patches[k] = PatchInfo.deserialize(bv, k, v)
  return patches

def save_patch(bv, p):
  global patches
  ps = bv.query_metadata("vibes.patch-infos")
  ps[p.name] = p.serialize(bv)
  patches[p.name] = p
  bv.store_metadata("vibes.patch-infos", ps)

def delete_patch(bv, name):
  global patches
  ps = bv.query_metadata("vibes.patch-infos")
  del ps[name]
  del patches[name]
  bv.store_metadata("vibes.patch-infos", ps)
  try:
    ps = bv.query_metadata("vibes.patch-codes")
    del ps[name]
    bv.store_metadata("vibes.patch-codes", ps)
  except KeyError:
    pass

def get_spaces(bv):
  global spaces
  try:
    ss = bv.query_metadata("vibes.patch-spaces")
  except KeyError:
    ss = []
    bv.store_metadata("vibes.patch-spaces", ss)
  if not spaces:
    for start, end in ss:
      spaces.append(AddressRange(start, end))
  return spaces

def save_space(bv, space):
  global spaces
  ss = bv.query_metadata("vibes.patch-spaces")
  ss.append((space.start, space.end))
  spaces.append(space)
  bv.store_metadata("vibes.patch-spaces", ss)

def delete_space(bv, space):
  try:
    global spaces
    spaces.remove(space)
  except ValueError:
    pass
  ss = filter(lambda s: s[0] != space.start or s[1] != space.end,
              bv.query_metadata("vibes.patch-spaces"))
  bv.store_metadata("vibes.patch-spaces", list(ss))

def get_patch_code(bv, name):
  try:
    ps = bv.query_metadata("vibes.patch-codes")
  except KeyError:
    ps = {name: ""}
    bv.store_metadata("vibes.patch-codes", ps)
  return ps.get(name, "")

def save_patch_code(bv, name, code):
  ps = bv.query_metadata("vibes.patch-codes")
  ps[name] = code
  bv.store_metadata("vibes.patch-codes", ps)

def get_ogre_functions(bv):
  try:
    fs = bv.query_metadata("vibes.ogre-functions")
  except KeyError:
    fs = []
    bv.store_metadata("vibes.ogre-functions", fs)
  return fs

def save_ogre_func(bv, f):
  fs = bv.query_metadata("vibes.ogre-functions")
  fs.append(f)
  fs = list(set(fs))
  bv.store_metadata("vibes.ogre-functions", fs)

def remove_ogre_func(bv, f):
  fs = bv.query_metadata("vibes.ogre-functions")
  try:
    fs.remove(f)
    bv.store_metadata("vibes.ogre-functions", fs)
  except ValueError:
    pass
