(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Core_kernel
open Bap.Std
open Bap_core_theory

module Hvar = Higher_var

open KB.Let

let err msg = Kb_error.fail @@ Kb_error.Higher_vars_not_substituted msg

exception Subst_err of string

module Naming = struct

  (* BAP seems to prefix an underscore when it's just `$reg` so we will
     just make it explicit. *)
  let reg_prefix = "_$reg"

  let mark_reg_name (v : string) : string = reg_prefix ^ v

  let mark_reg (v : var) : var =
    let name = Var.name v in
    let typ = Var.typ v in
    Var.create (mark_reg_name name) typ

  (* Find a register with a given name in a target arch. *)
  let mark_reg_exn (tgt : Theory.target) (name : string) : var =
    Theory.Target.regs tgt |>
    Set.find ~f:(fun v -> String.(Theory.Var.name v = name)) |>
    function
    | Some r -> mark_reg @@ Var.reify r
    | None ->
      raise
        (Subst_err
           (Format.sprintf "Register %s not found in target arch!" name))

  let unmark_reg_name (v : string) : string option =
    if String.is_prefix v ~prefix:reg_prefix then
      Some (String.drop_prefix v @@ String.length reg_prefix)
    else None

  let unmark_reg (v : var) : var option =
    let name = Var.name v in
    let typ = Var.typ v in
    match unmark_reg_name name with
    | Some name -> Some (Var.create name typ)
    | None -> None

end

let get_mem tgt =
  let mem = Theory.Target.data tgt in
  Var.reify mem

let size_of_typ (typ : typ) (name : string) : size =
  match typ with
  | Bil.Imm n -> Size.of_int n |> Or_error.ok_exn
  | _ -> raise @@ Subst_err (
      Format.sprintf "Unexpected type for variable: %s" name)

(* Initialize variables with their `at-entry` values. *)
let initialize
    (blks : blk term list)
    ~(typeof : string -> typ option)
    ~(entry_tid : tid)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target)
    ~(spilled : String.Set.t) : blk term list KB.t =
  KB.List.map blks ~f:(fun blk ->
      let tid = Term.tid blk in
      if Tid.(tid = entry_tid) then
        let+ defs = KB.List.filter_map hvars ~f:(fun hvar ->
            let name = Hvar.name hvar in
            if Set.mem spilled name then KB.return None
            else match Hvar.value hvar with
              | Hvar.Constant _ -> KB.return None
              | Hvar.Memory _ -> KB.return None
              | Hvar.Storage {at_entry = None; _} -> KB.return None
              | Hvar.Storage {at_entry = Some reg; _} ->
                match typeof name with
                | None -> KB.return None
                | Some typ ->
                  let lhs = Var.create name typ in
                  let rhs = Naming.mark_reg_exn tgt reg in
                  let+ tid = Theory.Label.fresh in
                  Some (Def.create ~tid lhs @@ Var rhs)) in
        (* Order these defs after we preserve any registers that were
           spilled. *)
        match
          Term.enum def_t blk |>
          Seq.to_list_rev |>
          List.find_map ~f:(fun def ->
              if Term.has_attr def Bir_helpers.spill_tag
              then Some (Term.tid def)
              else None)
        with
        | Some after -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.append ~after def_t blk def)
        | None -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.prepend def_t blk def)
      else KB.return blk)

(* Finalize variables with their `at-exit` values. *)
let finalize
    (blks : blk term list)
    ~(typeof : string -> typ option)
    ~(exit_tids : Tid.Set.t)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : blk term list KB.t =
  let* exit_tids =
    let+ blks = Bir_helpers.exit_blks blks in
    List.map blks ~f:Term.tid |> Tid.Set.of_list in
  KB.List.map blks ~f:(fun blk ->
      let tid = Term.tid blk in
      if Set.mem exit_tids tid then
        let+ defs = KB.List.filter_map hvars ~f:(fun hvar ->
            let name = Hvar.name hvar in
            match Hvar.value hvar with
            | Hvar.Constant _ -> KB.return None
            | Hvar.Memory _ -> KB.return None
            | Hvar.Storage {at_exit = None; _} -> KB.return None
            | Hvar.Storage {at_exit = Some reg; _} ->
              match typeof name with
              | None -> KB.return None
              | Some typ ->
                let rhs = Bil.var @@ Var.create name typ in
                let lhs = Naming.mark_reg_exn tgt reg in
                let+ tid = Theory.Label.fresh in
                Some (Def.create ~tid lhs rhs)) in
        (* Order these defs before we restore any registers that were
           spilled. *)
        match
          Term.enum def_t blk |>
          Seq.find_map ~f:(fun def ->
              if Term.has_attr def Bir_helpers.spill_tag
              then Some (Term.tid def)
              else None)
        with
        | Some before ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.prepend ~before def_t blk def)
        | None ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.append def_t blk def)
      else KB.return blk)

(* Substitute the name with an appropriate expression. Since we have the
   initialization and finalization done, we only need to worry about
   constants. *)
let subst_name
    (name : string)
    ~(typ : typ)
    ~(hvar : Hvar.t)
    ~(tgt : Theory.target) : exp =
  match Hvar.value hvar with
  | Hvar.Constant const -> Bil.int const
  | Hvar.Storage _ -> Bil.var @@ Var.create name typ
  | Hvar.Memory memory ->
    let mem = get_mem tgt in
    let size = size_of_typ typ name in
    let endian =
      let e = Theory.Target.endianness tgt in
      if Theory.Endianness.(e = le) then LittleEndian else BigEndian in
    match memory with
    | Frame (loc, off) ->
      let loc = Naming.mark_reg_exn tgt loc in
      Bil.(load ~mem:(var mem) ~addr:(var loc + int off)
             endian size)
    | Global addr ->
      Bil.(load ~mem:(var mem) ~addr:(int addr) endian size)

let subst_var
    (v : var)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : exp =
  let name = Var.name v in
  let typ = Var.typ v in
  match Hvar.find name hvars with
  | Some hvar -> subst_name name ~typ ~hvar ~tgt
  | None -> Bil.var v

let subst_exp
    (e : exp)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : exp =
  let obj = object
    inherit Exp.mapper
    method! map_var v = subst_var v ~hvars ~tgt
  end in
  obj#map_exp e

let subst_def
    (def : def term)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : def term =
  let lhs = Def.lhs def in
  let typ = Var.typ lhs in
  let name = Var.name lhs in
  let rhs = Def.rhs def |> subst_exp ~hvars ~tgt in
  match Hvar.find name hvars with
  | None -> Def.with_rhs def rhs
  | Some hvar -> match Hvar.value hvar with
    | Hvar.Constant _ -> raise @@ Subst_err (
        sprintf "Higher var %s appeared on the LHS of a def, but is \
                 given a constant value" name)
    | Hvar.Storage _ -> Def.with_rhs def rhs
    | Hvar.Memory memory ->
      let mem = get_mem tgt in
      let lhs = mem in
      let size = size_of_typ typ name in
      let endian =
        let e = Theory.Target.endianness tgt in
        if Theory.Endianness.(e = le) then LittleEndian else BigEndian in
      let rhs = match memory with
        | Frame (loc, off) ->
          let loc = Naming.mark_reg_exn tgt loc in
          Bil.(store ~mem:(var mem) ~addr:(var loc + int off)
                 rhs endian size)
        | Global addr ->
          Bil.(store ~mem:(var mem) ~addr:(int addr) rhs endian size) in
      Def.create ~tid:(Term.tid def) lhs rhs

let subst_label
    (label : label)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : label KB.t = match label with
  | Indirect _ -> KB.return label
  | Direct tid ->
    let+ name = KB.collect Theory.Label.name tid in
    match name with
    | None -> label
    | Some name -> match Hvar.find name hvars with
      | None -> label
      | Some hvar ->
        let typ = Type.Imm (Theory.Target.code_addr_size tgt) in
        Indirect (subst_name name ~typ ~hvar ~tgt)

let subst_dsts
    (jmp : jmp term)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : jmp term KB.t =
  let tid = Term.tid jmp in
  let cond = Jmp.cond jmp in
  match Jmp.kind jmp with
  | Goto label ->
    let+ label = subst_label label ~hvars ~tgt in
    Jmp.create_goto ~cond ~tid label
  | Call call -> begin
      let* target =
        Call.target call |>
        subst_label ~hvars ~tgt in
      match Call.return call with
      | None ->
        let call = Call.create ~target () in
        KB.return @@ Jmp.create_call ~cond ~tid call 
      | Some return ->
        let+ return = subst_label return ~hvars ~tgt in
        let call = Call.create ~target ~return () in
        Jmp.create_call ~cond ~tid call
    end
  | Ret label ->
    let+ label = subst_label label ~hvars ~tgt in
    Jmp.create_ret ~cond ~tid label
  | Int _ -> KB.return jmp

let subst_jmp
    (jmp : jmp term)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : jmp term KB.t =
  Jmp.map_exp jmp ~f:(subst_exp ~hvars ~tgt) |>
  subst_dsts ~hvars ~tgt

let subst_blk
    (blk : blk term)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target)
    ~(spilled : String.Set.t) : blk term KB.t =
  let+ jmps =
    Term.enum jmp_t blk |> Seq.to_list |>
    KB.List.map ~f:(subst_jmp ~hvars ~tgt) in
  (* This pass should be run before we convert to SSA form, so
     just do nothing with the phi nodes. *)
  let phis = Term.enum phi_t blk |> Seq.to_list in
  let defs =
    Term.enum def_t blk |> Seq.to_list |>
    List.map ~f:(subst_def ~hvars ~tgt) in
  Blk.create () ~phis ~defs ~jmps ~tid:(Term.tid blk)

let substitute
    ?(spilled : String.Set.t = String.Set.empty)
    (blks : blk term list)
    ~(entry_tid : tid)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : blk term list KB.t =
  try
    let* exit_tids =
      let+ exits = Bir_helpers.exit_blks blks in
      List.map exits ~f:Term.tid |> Tid.Set.of_list in
    let typeof =
      let env = List.fold blks ~init:String.Map.empty ~f:(fun env blk ->
          let free = Blk.free_vars blk in
          let lhs =
            Term.enum def_t blk |>
            Seq.fold ~init:Var.Set.empty ~f:(fun lhs def ->
                Set.add lhs @@ Def.lhs def) in
          Var.Set.union free lhs |>
          Set.fold ~init:env ~f:(fun env v ->
              Map.set env ~key:(Var.name v) ~data:(Var.typ v))) in
      (* NOTE: the variable might not have been mentioned in the
         program. *)
      Map.find env in
    let* blks =
      initialize blks ~typeof ~entry_tid ~hvars ~tgt ~spilled in
    let* blks =
      finalize blks ~typeof ~exit_tids ~hvars ~tgt in
    KB.List.map blks ~f:(subst_blk ~hvars ~tgt ~spilled)
  with Subst_err msg -> err msg

