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
  ps = bv.query_metadata("vibes.patch-infos")
  ps[p.name] = p.serialize(bv)
  bv.store_metadata("vibes.patch-infos", ps)

def delete_patch(bv, name):
  ps = bv.query_metadata("vibes.patch-infos")
  del ps[name]
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
  ss = bv.query_metadata("vibes.patch-spaces")
  ss.append((space.start, space.end))
  bv.store_metadata("vibes.patch-spaces", ss)

def delete_space(bv, space):
  ss = bv.query_metadata("vibes.patch-spaces")
  try:
    ss.remove(space)
    bv.store_metadata("vibes.patch-spaces", ss)
  except ValueError:
    pass

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
