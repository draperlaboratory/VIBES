module Ir = Vibes_ir.Types

type eff = {
  data : Ir.Operation.t list;
  ctrl : Ir.Operation.t list;
  ir : Ir.t;
}

type pure = {
  value : Ir.Operand.t;
  eff : eff;
}

let empty_eff : eff = {
  data = [];
  ctrl = [];
  ir = Ir.empty;
}

let (@.) (x : eff) (y : eff) : eff = {
  data = x.data @ y.data;
  ctrl = x.ctrl @ y.ctrl;
  ir = Ir.union x.ir y.ir;
}

let instr (i : Ir.Operation.t) (eff : eff) : eff =
  {eff with data = i :: eff.data}

let control (i : Ir.Operation.t) (eff : eff) : eff =
  {eff with ctrl = i :: eff.ctrl}
