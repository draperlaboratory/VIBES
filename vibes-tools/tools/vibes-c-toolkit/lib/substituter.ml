open Core_kernel
open Bap.Std
open Bap_core_theory

open KB.Let

module Hvar = Higher_var
module Log = Vibes_log_lib.Stream

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

let size_of_typ (typ : typ) (name : string) =
  match typ with
  | Bil.Imm n -> Size.of_int n |> Or_error.ok_exn
  | _ ->
    raise
      (Subst_err
         (Format.sprintf "Unexpected type for variable: %s" name))

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
              | Hvar.Registers {at_entry = None; _} -> KB.return None
              | Hvar.Registers {at_entry = Some reg; _} ->
                match typeof name with
                | None ->
                  Log.send (sprintf "Warning: unused variable %s" name);
                  KB.return None
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
            | Hvar.Registers {at_exit = None; _} -> KB.return None
            | Hvar.Registers {at_exit = Some reg; _} ->
              match typeof name with
              | None ->
                Log.send (sprintf "Warning: unused variable %s" name);
                KB.return None
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

let subst_name (tgt : Theory.target) (t : Hvar.t)
    (name : string) (typ : typ) : exp =
  match Hvar.value t with
  | Hvar.Constant const -> Bil.int const
  | Hvar.Registers _ -> Bil.var @@ Var.create name typ
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

(* This replaces a variable with either the register or the memory
   read it corresponds to *)
let subst_var (tgt : Theory.target) (h_vars : Hvar.t list) (v : var) : exp =
  let name = Var.name v in
  let typ = Var.typ v in
  match Hvar.find name h_vars with
  | Some hvar -> subst_name tgt hvar name typ
  | None -> Bil.var v

let subst_exp (tgt : Theory.target) (h_vars : Hvar.t list) (e : exp) : exp =
  let subst_obj =
    object
      inherit Exp.mapper
      method! map_var v =
        subst_var tgt h_vars v
    end
  in
  subst_obj#map_exp e

let subst_def
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (ir : def term)
  : def term =
  let lhs = Def.lhs ir in
  let typ = Var.typ lhs in
  let name = Var.name lhs in
  let rhs = Def.rhs ir |> subst_exp tgt h_vars in
  match Hvar.find name h_vars with
  | None -> Def.with_rhs ir rhs
  | Some hvar -> match Hvar.value hvar with
    | Hvar.Constant _ -> raise @@ Subst_err (
        sprintf "Higher var %s appeared on the LHS of a def, but is \
                 given a constant value" name)
    | Hvar.Registers _ -> Def.with_rhs ir rhs
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
      Def.create ~tid:(Term.tid ir) lhs rhs

let subst_label
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (label : label) : label KB.t =
  match label with
  | Indirect _ -> KB.return label
  | Direct tid ->
    let+ name = KB.collect Theory.Label.name tid in
    match name with
    | None -> label
    | Some name -> match Hvar.find name h_vars with
      | None -> label
      | Some hvar ->
        let typ = Type.Imm (Theory.Target.code_addr_size tgt) in
        Indirect (subst_name tgt hvar name typ)

let subst_dsts
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (jmp : jmp term) : jmp term KB.t =
  let tid = Term.tid jmp in
  let cond = Jmp.cond jmp in
  match Jmp.kind jmp with
  | Goto label ->
    let+ label = subst_label tgt h_vars label in
    Jmp.create_goto ~cond ~tid label
  | Call call -> begin
      let* target =
        Call.target call |>
        subst_label tgt h_vars in
      match Call.return call with
      | None ->
        let call = Call.create ~target () in
        KB.return @@ Jmp.create_call ~cond ~tid call 
      | Some return ->
        let+ return = subst_label tgt h_vars return in
        let call = Call.create ~target ~return () in
        Jmp.create_call ~cond ~tid call
    end
  | Ret label ->
    let+ label = subst_label tgt h_vars label in
    Jmp.create_ret ~cond ~tid label
  | Int _ -> KB.return jmp

let subst_jmp
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (ir : jmp term) : jmp term KB.t =
  Jmp.map_exp ir ~f:(subst_exp tgt h_vars) |> subst_dsts tgt h_vars

let subst_blk
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (ir : blk term) : blk term KB.t =
  let+ jmps =
    Term.enum jmp_t ir |>
    Seq.to_list |>
    KB.List.map ~f:(subst_jmp tgt h_vars) in
  (* This pass should be run before we convert to SSA form, so
     just do nothing with the phi nodes. *)
  let phis = Term.enum phi_t ir |> Seq.to_list in
  let defs =
    Term.enum def_t ir |>
    Seq.to_list |>
    List.map ~f:(subst_def tgt h_vars) in
  Blk.create () ~phis ~defs ~jmps ~tid:(Term.tid ir)

let substitute
    ?(spilled : String.Set.t = String.Set.empty)
    (blks : blk term list)
    ~(entry_tid : tid)
    ~(hvars : Hvar.t list)
    ~(tgt : Theory.target) : blk term list KB.t =
  try
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
      finalize blks ~typeof ~hvars ~tgt in
    KB.List.map blks ~f:(subst_blk tgt hvars)
  with Subst_err msg -> err msg
