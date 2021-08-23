open !Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

open Bap_core_theory

let err msg = Kb_error.fail @@ Kb_error.Higher_vars_not_substituted msg

(* We use an exception here to avoid annoying monadic piping. *)
exception Subst_err of string

(* Find a register with a given name in a target arch. *)
let get_reg tgt name =
  let regs = Theory.Target.regs tgt in
  let reg = Set.find regs
      ~f:(fun v -> String.(Theory.Var.name v = name))
  in
  match reg with
  | Some r -> Var.reify r
  | None -> raise
              (Subst_err
                 (Format.sprintf "Register %s not found in target arch!" name))

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

(* This replaces a variable with either the register or the memory
   read it corresponds to *)
let subst_var (tgt : Theory.target) (h_vars : Hvar.t list) (v : var) : exp =
  let name = Var.name v in
  let typ = Var.typ v in
  match Hvar.find name h_vars with
  | Some t ->
    begin
      match Hvar.at_entry t with
      | Register name -> Bil.var @@ get_reg tgt name
      | Memory(loc, off) ->
        let mem = get_mem tgt |> Bil.var in
        let endianness =
          let e = Theory.Target.endianness tgt in
          if Theory.Endianness.(e = le) then
            LittleEndian
          else
            BigEndian
        in
        let size = size_of_typ typ name in
        let loc = get_reg tgt loc in
        Bil.load ~mem:mem ~addr:Bil.(var loc + int off) endianness size
    end
  | _ -> Var v

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
  let rhs = Def.rhs ir |> subst_exp tgt h_vars in
  let name = Var.name lhs in
  match Hvar.find name h_vars with
  | None -> Def.with_rhs ir rhs
  | Some t ->
    begin
      match Hvar.at_entry t with
      | Register name ->
        let lhs = get_reg tgt name in
        Def.create ~tid:(Term.tid ir) lhs rhs
      | Memory (loc, off) ->
        let mem = get_mem tgt in
        let lhs = mem in
        let loc = get_reg tgt loc in
        let endianness =
          let e = Theory.Target.endianness tgt in
          if Theory.Endianness.(e = le) then
            LittleEndian
          else
            BigEndian
        in
        let size = size_of_typ typ name in
        let rhs =
          Bil.
            (store ~mem:(var mem) ~addr:(var loc + int off) rhs endianness size)
        in
        Def.create ~tid:(Term.tid ir) lhs rhs
    end

let subst_jmp (tgt : Theory.target) (h_vars : Hvar.t list) (ir : jmp term) : jmp term =
  Jmp.map_exp ir ~f:(subst_exp tgt h_vars)

let subst_blk (tgt : Theory.target) (h_vars : Hvar.t list) (ir : blk term) : blk term =
  Blk.map_elts ir
    ~phi:(fun _ -> failwith "subst_blk: Unexpected Phi node!")
    ~def:(fun d -> subst_def tgt h_vars d)
    ~jmp:(fun j -> subst_jmp tgt h_vars j)


let substitute
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (ir : blk term list)
  : blk term list KB.t =
  try
    KB.return @@ List.map ~f:(subst_blk tgt h_vars) ir
  with
  | Subst_err msg -> err msg