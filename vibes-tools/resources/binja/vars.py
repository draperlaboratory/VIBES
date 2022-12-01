from binaryninja import *

# Higher-level variables
class HigherVar:
  REG_VAR = 0
  FRAME_VAR = 1
  GLOBAL_VAR = 2
  FUNCTION_VAR = 3
  CONSTANT_VAR = 4

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
    elif self.type == HigherVar.CONSTANT_VAR:
      return "Constant"
    else:
      assert False

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
    elif self.type == HigherVar.CONSTANT_VAR:
      # Higher vars shouldn't infer this
      assert False
    else:
      assert False


# Patch variables
class PatchVar(HigherVar):
  def __init__(self, name, value, type, default_reg=True):
    if type == HigherVar.REG_VAR and default_reg:
      value = (value, value)
    super(PatchVar, self).__init__(name, value, type)

  def set_at_entry(self, r):
    if self.type == HigherVar.REG_VAR:
      self.value = (r, self.value[1])

  def set_at_exit(self, r):
    if self.type == HigherVar.REG_VAR:
      self.value = (self.value[0], r)

  def value_str(self):
    # not valid for PatchVar
    assert False

  def serialize(self, bv):
    storage = self._serialize_storage_class(bv)
    return {"name": self.name, "storage-class": storage}

  def _serialize_storage_class(self, bv):
    sz = bv.arch.address_size * 8
    if self.type == HigherVar.REG_VAR:
      reg = {}
      if self.value[0] is not None:
        reg["at-entry"] = self.value[0]
      if self.value[1] is not None:
        reg["at-exit"] = self.value[1]
      return ["register", reg]
    elif self.type == HigherVar.FRAME_VAR:
      reg, off_int = self.value
      if off_int < 0:
        off = "-0x%x:%d" % (-off_int, sz)
      else:
        off = "0x%x:%d" % (off_int, sz)
      return ["memory", ["frame", reg, off]]
    elif self.type == HigherVar.GLOBAL_VAR:
      return ["memory", ["address", "0x%x:%d" % (self.value, sz)]]
    elif self.type == HigherVar.FUNCTION_VAR:
      return ["constant", "0x%x:%d" % (self.value, sz)]
    elif self.type == HigherVar.CONSTANT_VAR:
      return ["constant", "0x%x:%d" % (self.value[0], self.value[1])]
    else:
      assert False

  @staticmethod
  def deserialize(bv, d):
    name = d["name"]
    type, value = PatchVar._deserialize_storage_class(bv, d["storage-class"])
    return PatchVar(name, value, type, default_reg=False)

  @staticmethod
  def _deserialize_storage_class(bv, d):
    if d[0] == "register":
      type = HigherVar.REG_VAR
      value = (d[1].get("at-entry"), d[1].get("at-exit"))
    elif d[0] == "memory":
      m = d[1]
      if m[0] == "frame":
        type = HigherVar.FRAME_VAR
        value = m[1], int(m[2].split(":")[0], base=16)
      elif m[0] == "address":
        type = HigherVar.GLOBAL_VAR
        value = int(m[1].split(":")[0], base=16)
      else:
        assert False
    elif d[0] == "constant":
      value = int(d[1].split(":")[0], base=16)
      type = HigherVar.FUNCTION_VAR if \
        bv.get_functions_containing(value) \
        else HigherVar.CONSTANT_VAR
      if type == HigherVar.CONSTANT_VAR:
        size_str = d[1].split(":")[1]
        if size_str.endswith("u"):
          size_str = size_str[:-1]
        size = int(size_str)
        if size < 1:
          size = 1
        elif size == 3:
          size = 4
        elif size > 4:
          size = 8
        value = (value, size)
    else:
      assert False
    return (type, value)
