open Core
open Bap.Std
open Vibes_ir.Types

module Naming = Vibes_higher_vars.Substituter.Naming
module Linear = Vibes_linear_ssa.Utils
module Tags = Vibes_bir.Tags

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

module Call_params = struct

  type info = {
    ops : Operand.t list;
    ignored : Tid.Set.t;
  }

  let empty_info : info = {
    ops = [];
    ignored = Tid.Set.empty;
  }

  type t = info Tid.Map.t

  let collect (sub : sub term) : t =
    Term.enum blk_t sub |>
    Seq.fold ~init:Tid.Map.empty ~f:(fun m blk ->
        let info =
          Term.enum def_t blk |>
          Seq.fold ~init:empty_info ~f:(fun info def ->
              if Term.has_attr def Tags.argument then
                let lhs = Def.lhs def in
                match Var.typ lhs, Def.rhs def with
                | Imm _, _ ->
                  let v = Operand.Var (Opvar.create lhs) in
                  {info with ops = v :: info.ops}
                | Mem _, Var m ->
                  (* We do not want to actually generate code for this.
                     It is just a signpost for the selector to collect the
                     most recent version of the memory so we can pass it as
                     a dependency of the call. *)
                  let m = Operand.Void (Opvar.create m) in
                  let ignored = Set.add info.ignored @@ Term.tid def in
                  {ops = m :: info.ops; ignored}
                | _ -> info
              else info) in
        Map.set m ~key:(Term.tid blk) ~data:info)

end
