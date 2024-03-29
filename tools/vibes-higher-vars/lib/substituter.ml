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

open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Bir_helpers = Vibes_bir.Helpers
module Tags = Vibes_bir.Tags
module Hvar = Higher_var

open KB.Syntax

exception Subst_err of string

module Naming = struct

  let reg_prefix = "reg:"

  let mark_reg_name_unsafe : string -> string =
    Format.sprintf "%s%s" reg_prefix

  let mark_reg_unsafe (v : var) : var =
    let name = Format.sprintf "%s%s" reg_prefix @@ Var.name v in
    Var.reify @@ T.Var.define (Var.sort v) name

  let mark_reg_name : T.target -> string -> (string, string) result =
    let msg name target =
      Format.asprintf "Register %s not found in target %a!"
        name T.Target.pp target in
    fun target name ->
      T.Target.regs target |> Set.exists ~f:(fun r ->
          String.equal name Var.(name @@ reify r)) |> function
      | true -> Ok (reg_prefix ^ name)
      | false -> Error (msg name target)

  let mark_reg
      (target : Theory.target)
      (v : var) : (var, string) result =
    Var.name v |> mark_reg_name target |>
    Result.map ~f:(fun name -> Var.reify @@ T.Var.define (Var.sort v) name)

  let is_reg_name : string -> bool = String.is_prefix ~prefix:reg_prefix
  let is_reg : var -> bool = Fn.compose is_reg_name Var.name

  let unmark_reg_name (v : string) : string option =
    if is_reg_name v then
      Some (String.drop_prefix v @@ String.length reg_prefix)
    else None

  let unmark_reg (v : var) : var option =
    let name = Var.name v in
    let typ = Var.typ v in
    match unmark_reg_name name with
    | Some name -> Some (Var.create name typ)
    | None -> None

end

let get_mem target =
  let mem = T.Target.data target in
  Var.reify mem

let size_of_typ : typ -> string -> size =
  let msg name = Format.asprintf "Unexpected type for variable: %s" name in
  fun typ name -> match typ with
    | Bil.Imm n -> Size.of_int n |> Or_error.ok_exn
    | _ -> raise @@ Subst_err (msg name)

(* Initialize variables with their `at-entry` values. *)
let initialize
    (sub : sub term)
    ~(typeof : string -> typ option)
    ~(hvars : Hvar.t list)
    ~(target : T.target) : sub term KB.t =
  let default_typ = Type.Imm (T.Target.bits target) in
  let* entry_tid = match Bir_helpers.entry_tid sub with
    | Error err -> KB.fail err
    | Ok tid -> !!tid in
  Term.KB.map blk_t sub ~f:(fun blk ->
      let tid = Term.tid blk in
      if Tid.(tid = entry_tid) then
        let+ defs = KB.List.filter_map hvars ~f:(fun {name; value; _} ->
            (* If this variable got spilled then it should have the Memory
               classifier. *)
            match value with
            | Hvar.Constant _ -> !!None
            | Hvar.Memory _ -> !!None
            | Hvar.Preassign _ -> !!None
            | Hvar.Registers {at_entry = None; _} -> !!None
            | Hvar.Registers {at_entry = Some reg; _} ->
              let typ = typeof name |> Option.value ~default:default_typ in
              let lhs = Var.create name typ in
              match Naming.mark_reg_name target reg with
              | Error msg ->
                KB.fail @@ Errors.Higher_var_not_substituted msg
              | Ok name ->
                let rhs = Var.create name typ in
                let+ tid = T.Label.fresh in
                Some (Def.create ~tid lhs @@ Var rhs)) in
        (* Order these defs after we preserve any registers that were
           spilled. *)
        let last_spill =
          Term.enum def_t blk |> Seq.to_list_rev |>
          List.find_map ~f:(fun def ->
              if Term.has_attr def Tags.spill
              then Some (Term.tid def)
              else None) in
        match last_spill with
        | Some after -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.append ~after def_t blk def)
        | None -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.prepend def_t blk def)
      else !!blk)

(* Finalize variables with their `at-exit` values. *)
let finalize
    (sub : sub term)
    ~(typeof : string -> typ option)
    ~(hvars : Hvar.t list)
    ~(target : T.target) : sub term KB.t =
  let default_typ = Type.Imm (T.Target.bits target) in
  Term.KB.map blk_t sub ~f:(fun blk ->
      (* The implicit exit block will have the finalizations. All other
         exit blocks are assumed to go somewhere else in the program
         where the storage information is not needed. *)
      if Bir_helpers.no_jmps blk then
        let+ defs = KB.List.filter_map hvars ~f:(fun {name; value; _} ->
            match value with
            | Hvar.Constant _ -> !!None
            | Hvar.Memory _ -> !!None
            | Hvar.Preassign _ -> !!None
            | Hvar.Registers {at_exit = None; _} -> !!None
            | Hvar.Registers {at_exit = Some reg; _} ->
              let typ = typeof name |> Option.value ~default:default_typ in
              let rhs = Bil.var @@ Var.create name typ in
              match Naming.mark_reg_name target reg with
              | Error msg ->
                KB.fail @@ Errors.Higher_var_not_substituted msg
              | Ok name ->
                let lhs = Var.create name typ in
                let+ tid = T.Label.fresh in
                Some (Def.create ~tid lhs rhs)) in
        (* Order these defs before we restore any registers that were
           spilled. *)
        let first_spill =
          Term.enum def_t blk |> Seq.find_map ~f:(fun def ->
              if Term.has_attr def Tags.spill
              then Some (Term.tid def)
              else None) in
        match first_spill with
        | Some before ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.prepend ~before def_t blk def)
        | None ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.append def_t blk def)
      else !!blk)

let subst_name
    (target : T.target)
    (hvar : Hvar.t)
    (name : string)
    (typ : typ) : exp =
  match hvar.value with
  | Hvar.Constant const -> Bil.int const
  | Hvar.Registers _ -> Bil.var @@ Var.create name typ
  | Hvar.Preassign reg ->
    let width = T.Target.bits target in
    let typ = Type.Imm width in
    let v = Var.create reg typ in
    begin match Naming.mark_reg target v with
      | Error msg -> raise @@ Subst_err msg
      | Ok r -> Bil.var r
    end
  | Hvar.Memory memory ->
    let mem = get_mem target in
    let size = size_of_typ typ name in
    let endian =
      let e = T.Target.endianness target in
      if T.Endianness.(e = le) then LittleEndian else BigEndian in
    match memory with
    | Global addr ->
      Bil.(load ~mem:(var mem) ~addr:(int addr) endian size)
    | Frame (loc, off) -> match Naming.mark_reg_name target loc with
      | Error msg -> raise (Subst_err msg)
      | Ok name ->
        let width = T.Target.bits target in
        let loc = Var.create name @@ Imm width in
        Bil.(load ~mem:(var mem) ~addr:(var loc + int off) endian size)

(* This replaces a variable with either the register or the memory
   read it corresponds to *)
let subst_var (target : T.target) (hvars : Hvar.t list) (v : var) : exp =
  let name = Var.name v in
  match Hvar.find name hvars with
  | Some hvar -> subst_name target hvar name @@ Var.typ v
  | None -> Bil.var v

let subst_exp (target : T.target) (hvars : Hvar.t list) (e : exp) : exp =
  (object
    inherit Exp.mapper
    method! map_var v = subst_var target hvars v
  end)#map_exp e

let subst_def : T.target -> Hvar.t list -> def term -> def term =
  let bad_const name =
    Format.asprintf
      "Higher var %s appeared on the LHS of a def, but is \
       given a constant value" name in
  fun target hvars def ->
    let lhs = Def.lhs def in
    let typ = Var.typ lhs in
    let name = Var.name lhs in
    let rhs = Def.rhs def |> subst_exp target hvars in
    match Hvar.find name hvars with
    | None -> Def.with_rhs def rhs
    | Some hvar -> match hvar.value with
      | Hvar.Constant _ -> raise @@ Subst_err (bad_const name)
      | Hvar.Registers _ -> Def.with_rhs def rhs
      | Hvar.Preassign reg ->
        let width = T.Target.bits target in
        let typ = Type.Imm width in
        let v = Var.create reg typ in
        let r = match Naming.mark_reg target v with
          | Error msg -> raise @@ Subst_err msg
          | Ok r -> r in
        Def.with_lhs (Def.with_rhs def rhs) r
      | Hvar.Memory memory ->
        let mem = get_mem target in
        let lhs = mem in
        let size = size_of_typ typ name in
        let endian =
          let e = T.Target.endianness target in
          if T.Endianness.(e = le) then LittleEndian else BigEndian in
        let rhs = match memory with
          | Global addr ->
            Bil.(store ~mem:(var mem) ~addr:(int addr) rhs endian size)
          | Frame (loc, off) -> match Naming.mark_reg_name target loc with
            | Error msg -> raise (Subst_err msg)
            | Ok name ->
              let width = T.Target.bits target in
              let loc = Var.create name @@ Imm width in
              Bil.(store ~mem:(var mem) ~addr:(var loc + int off)
                     rhs endian size) in
        let attrs = Term.attrs def in
        Term.with_attrs (Def.create ~tid:(Term.tid def) lhs rhs) attrs

let subst_label
    (target : T.target)
    (hvars : Hvar.t list) : label -> label KB.t = function
  | Indirect _ as label -> !!label
  | Direct tid as label ->
    let* name = KB.collect T.Label.name tid in
    match name with
    | None -> !!label
    | Some name -> match Hvar.find name hvars with
      | None -> !!label
      | Some hvar ->
        let typ = Type.Imm (T.Target.code_addr_size target) in
        !!(Indirect (subst_name target hvar name typ))

let subst_dsts
    (target : T.target)
    (hvars : Hvar.t list)
    (jmp : jmp term) : jmp term KB.t =
  let tid = Term.tid jmp in
  let cond = Jmp.cond jmp in
  match Jmp.kind jmp with
  | Goto label ->
    let+ label = subst_label target hvars label in
    Jmp.create_goto ~cond ~tid label
  | Call call ->
    let* return= match Call.return call with
      | Some return ->
        let+ return = subst_label target hvars return in
        Some return
      | None -> !!None in
    let+ target = Call.target call |> subst_label target hvars in
    let call = Call.create ?return ~target () in
    Jmp.create_call ~cond ~tid call
  | Ret label ->
    let+ label = subst_label target hvars label in
    Jmp.create_ret ~cond ~tid label
  | Int _ -> !!jmp

let subst_jmp
    (target : T.target)
    (hvars : Hvar.t list)
    (jmp : jmp term) : jmp term KB.t =
  let attrs = Term.attrs jmp in
  let+ jmp =
    Jmp.map_exp jmp ~f:(subst_exp target hvars) |>
    subst_dsts target hvars in
  Term.with_attrs jmp attrs

let subst_blk
    (target : T.target)
    (hvars : Hvar.t list)
    (blk : blk term) : blk term KB.t =
  let blk = Term.map def_t blk ~f:(subst_def target hvars) in
  Term.KB.map jmp_t blk ~f:(subst_jmp target hvars)

let substitute
    (sub : sub term)
    ~(hvars : Hvar.t list)
    ~(target : T.target) : sub term KB.t =
  let typeof =
    let env =
      Term.enum blk_t sub |>
      Seq.fold ~init:String.Map.empty ~f:(fun env blk ->
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
  let* sub = initialize sub ~typeof ~hvars ~target in
  let* sub = finalize sub ~typeof ~hvars ~target in
  try Term.KB.map blk_t sub ~f:(subst_blk target hvars)
  with Subst_err msg -> KB.fail @@ Errors.Higher_var_not_substituted msg
