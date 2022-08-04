open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log_lib.Stream
module Bir_helpers = Vibes_bir_lib.Helpers
module Hvar = Higher_var

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
  let mark_reg_exn : T.target -> string -> var =
    let msg name tgt =
      Format.asprintf "Register %s not found in target %a!"
        name T.Target.pp tgt in
    fun tgt name ->
      T.Target.regs tgt |>
      Set.find ~f:(fun v -> String.(T.Var.name v = name)) |> function
      | Some r -> mark_reg @@ Var.reify r
      | None -> raise @@ Subst_err (msg name tgt)

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
  let mem = T.Target.data tgt in
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
    ~(entry_tid : tid)
    ~(hvars : Hvar.t list)
    ~(tgt : T.target)
    ~(spilled : String.Set.t) : sub term =
  Term.map blk_t sub ~f:(fun blk ->
      let tid = Term.tid blk in
      if Tid.(tid = entry_tid) then
        let defs = List.filter_map hvars ~f:(fun hvar ->
            let name = Hvar.name hvar in
            if Set.mem spilled name then None
            else match Hvar.value hvar with
              | Hvar.Constant _ -> None
              | Hvar.Memory _ -> None
              | Hvar.Registers {at_entry = None; _} -> None
              | Hvar.Registers {at_entry = Some reg; _} ->
                match typeof name with
                | None ->
                  Log.send "Warning: unused variable '%s'" name;
                  None
                | Some typ ->
                  let lhs = Var.create name typ in
                  let rhs = Naming.mark_reg_exn tgt reg in
                  let tid = Tid.create () in
                  Some (Def.create ~tid lhs @@ Var rhs)) in
        (* Order these defs after we preserve any registers that were
           spilled. *)
        let last_spill =
          Term.enum def_t blk |> Seq.to_list_rev |>
          List.find_map ~f:(fun def ->
              if Term.has_attr def Bir_helpers.spill_tag
              then Some (Term.tid def)
              else None) in
        match last_spill with
        | Some after -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.append ~after def_t blk def)
        | None -> List.fold defs ~init:blk ~f:(fun blk def ->
            Term.prepend def_t blk def)
      else blk)

(* Finalize variables with their `at-exit` values. *)
let finalize
    (sub : sub term)
    ~(typeof : string -> typ option)
    ~(hvars : Hvar.t list)
    ~(tgt : T.target) : sub term =
  Term.map blk_t sub ~f:(fun blk ->
      (* The implicit exit block will have the finalizations. All other
         exit blocks are assumed to go somewhere else in the program
         where the storage information is not needed. *)
      if Bir_helpers.no_jmps blk then
        let defs = List.filter_map hvars ~f:(fun hvar ->
            let name = Hvar.name hvar in
            match Hvar.value hvar with
            | Hvar.Constant _ -> None
            | Hvar.Memory _ -> None
            | Hvar.Registers {at_exit = None; _} -> None
            | Hvar.Registers {at_exit = Some reg; _} ->
              match typeof name with
              | None ->
                Log.send "Warning: unused variable '%s'" name;
                None
              | Some typ ->
                let rhs = Bil.var @@ Var.create name typ in
                let lhs = Naming.mark_reg_exn tgt reg in
                let tid = Tid.create () in
                Some (Def.create ~tid lhs rhs)) in
        (* Order these defs before we restore any registers that were
           spilled. *)
        let first_spill =
          Term.enum def_t blk |> Seq.find_map ~f:(fun def ->
              if Term.has_attr def Bir_helpers.spill_tag
              then Some (Term.tid def)
              else None) in
        match first_spill with
        | Some before ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.prepend ~before def_t blk def)
        | None ->
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.append def_t blk def)
      else blk)

let subst_name
    (tgt : T.target)
    (t : Hvar.t)
    (name : string)
    (typ : typ) : exp =
  match Hvar.value t with
  | Hvar.Constant const -> Bil.int const
  | Hvar.Registers _ -> Bil.var @@ Var.create name typ
  | Hvar.Memory memory ->
    let mem = get_mem tgt in
    let size = size_of_typ typ name in
    let endian =
      let e = T.Target.endianness tgt in
      if T.Endianness.(e = le) then LittleEndian else BigEndian in
    match memory with
    | Frame (loc, off) ->
      let loc = Naming.mark_reg_exn tgt loc in
      Bil.(load ~mem:(var mem) ~addr:(var loc + int off)
             endian size)
    | Global addr ->
      Bil.(load ~mem:(var mem) ~addr:(int addr) endian size)

(* This replaces a variable with either the register or the memory
   read it corresponds to *)
let subst_var (tgt : T.target) (hvars : Hvar.t list) (v : var) : exp =
  let name = Var.name v in
  match Hvar.find name hvars with
  | Some hvar -> subst_name tgt hvar name @@ Var.typ v
  | None -> Bil.var v

let subst_exp (tgt : T.target) (hvars : Hvar.t list) (e : exp) : exp =
  (object
    inherit Exp.mapper
    method! map_var v = subst_var tgt hvars v
  end)#map_exp e

let subst_def : T.target -> Hvar.t list -> def term -> def term =
  let bad_const name =
    Format.asprintf
      "Higher var %s appeared on the LHS of a def, but is \
       given a constant value" name in
  fun tgt hvars def ->
    let lhs = Def.lhs def in
    let typ = Var.typ lhs in
    let name = Var.name lhs in
    let rhs = Def.rhs def |> subst_exp tgt hvars in
    match Hvar.find name hvars with
    | None -> Def.with_rhs def rhs
    | Some hvar -> match Hvar.value hvar with
      | Hvar.Constant _ -> raise @@ Subst_err (bad_const name)
      | Hvar.Registers _ -> Def.with_rhs def rhs
      | Hvar.Memory memory ->
        let mem = get_mem tgt in
        let lhs = mem in
        let size = size_of_typ typ name in
        let endian =
          let e = T.Target.endianness tgt in
          if T.Endianness.(e = le) then LittleEndian else BigEndian in
        let rhs = match memory with
          | Frame (loc, off) ->
            let loc = Naming.mark_reg_exn tgt loc in
            Bil.(store ~mem:(var mem) ~addr:(var loc + int off)
                   rhs endian size)
          | Global addr ->
            Bil.(store ~mem:(var mem) ~addr:(int addr) rhs endian size) in
        Def.create ~tid:(Term.tid def) lhs rhs

let subst_label
    (tgt : T.target)
    (hvars : Hvar.t list) : label -> label = function
  | Indirect _ as label -> label
  | Direct tid as label ->
    let name = Tid.name tid in
    match String.chop_prefix name ~prefix:"@" with
    | None -> label
    | Some name -> match Hvar.find name hvars with
      | None -> label
      | Some hvar ->
        let typ = Type.Imm (T.Target.code_addr_size tgt) in
        Indirect (subst_name tgt hvar name typ)

let subst_dsts
    (tgt : T.target)
    (hvars : Hvar.t list)
    (jmp : jmp term) : jmp term =
  let tid = Term.tid jmp in
  let cond = Jmp.cond jmp in
  match Jmp.kind jmp with
  | Goto label ->
    let label = subst_label tgt hvars label in
    Jmp.create_goto ~cond ~tid label
  | Call call -> begin
      let target =
        Call.target call |>
        subst_label tgt hvars in
      match Call.return call with
      | None ->
        let call = Call.create ~target () in
        Jmp.create_call ~cond ~tid call 
      | Some return ->
        let return = subst_label tgt hvars return in
        let call = Call.create ~target ~return () in
        Jmp.create_call ~cond ~tid call
    end
  | Ret label ->
    let label = subst_label tgt hvars label in
    Jmp.create_ret ~cond ~tid label
  | Int _ -> jmp

let subst_jmp
    (tgt : T.target)
    (hvars : Hvar.t list)
    (jmp : jmp term) : jmp term =
  Jmp.map_exp jmp ~f:(subst_exp tgt hvars) |> subst_dsts tgt hvars

let subst_blk
    (tgt : T.target)
    (hvars : Hvar.t list)
    (blk : blk term) : blk term =
  let jmps =
    Term.enum jmp_t blk |>
    Seq.to_list |>
    List.map ~f:(subst_jmp tgt hvars) in
  (* This pass should be run before we convert to SSA form, so
     just do nothing with the phi nodes. *)
  let phis = Term.enum phi_t blk |> Seq.to_list in
  let defs =
    Term.enum def_t blk |>
    Seq.to_list |>
    List.map ~f:(subst_def tgt hvars) in
  Blk.create () ~phis ~defs ~jmps ~tid:(Term.tid blk)

let substitute
    ?(spilled : String.Set.t = String.Set.empty)
    (sub : sub term)
    ~(entry_tid : tid)
    ~(hvars : Hvar.t list)
    ~(tgt : T.target) : (sub term, KB.Conflict.t) result =
  try
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
    initialize sub ~typeof ~entry_tid ~hvars ~tgt ~spilled |>
    finalize ~typeof ~hvars ~tgt |>
    Term.map blk_t ~f:(subst_blk tgt hvars) |>
    Result.return
  with Subst_err msg -> Error (Errors.Higher_var_not_substituted msg)
