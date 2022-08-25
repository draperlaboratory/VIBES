open Vibes_ir.Types

type eff = {
  data : Operation.t list;
  ctrl : Operation.t list;
  ir : t;
}

type pure = {
  value : Operand.t;
  eff : eff;
}

let empty_eff : eff = {
  data = [];
  ctrl = [];
  ir = empty;
}

let (@.) (x : eff) (y : eff) : eff = {
  data = x.data @ y.data;
  ctrl = x.ctrl @ y.ctrl;
  ir = union x.ir y.ir;
}

let instr (i : Operation.t) (eff : eff) : eff =
  {eff with data = i :: eff.data}

let control (i : Operation.t) (eff : eff) : eff =
  {eff with ctrl = i :: eff.ctrl}
