open Core
open Bap.Std
open Vibes_ir.Types

module Naming = Vibes_higher_vars.Substituter.Naming
module Linear = Vibes_linear_ssa.Utils

module Sel = struct

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

end

module Preassign = struct

  type transform = typ -> string -> var

  (* Assuming the var is in linear SSA form, we undo this and then
     check if it's a register name. *)
  let reg_name (v : var) : string option =
    let name = Var.name v in
    let name = Option.value ~default:name (Linear.orig_name name) in
    Naming.unmark_reg_name name

  let run (ir : t) ~(f : transform) : t =
    map_opvars ir ~f:(fun v ->
        let r = List.find_map v.temps ~f:(fun v ->
            reg_name v |> Option.map ~f:(fun name ->
                f (Var.typ v) name)) in
        let preassign = match r with
          | None -> v.preassign
          | Some _ -> r in
        {v with preassign})

end
