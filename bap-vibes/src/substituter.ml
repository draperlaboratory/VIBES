open !Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

open Bap_core_theory
open KB.Syntax

let err msg = Kb_error.fail @@ Kb_error.Higher_vars_not_substituted msg

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

let subst_name (tgt : Theory.target) (t : Hvar.t)
    (name : string) (typ : typ) : exp =
  match Hvar.value t with
  | Hvar.Storage {at_entry; _} -> begin
      match at_entry with
      | Hvar.Register name -> Bil.var @@ get_reg tgt name
      | Hvar.Memory memory ->
        let mem = get_mem tgt |> Bil.var in
        let endianness =
          let e = Theory.Target.endianness tgt in
          if Theory.Endianness.(e = le) then
            LittleEndian
          else
            BigEndian
        in
        let size = size_of_typ typ name in
        match memory with
        | Hvar.Frame (loc, off) ->
          let loc = get_reg tgt loc in
          Bil.load ~mem:mem ~addr:Bil.(var loc + int off) endianness size
        | Hvar.Global addr ->
          Bil.load ~mem ~addr:(Bil.int addr) endianness size
    end
  | Hvar.Constant const -> Bil.int const

(* This replaces a variable with either the register or the memory
   read it corresponds to *)
let subst_var (tgt : Theory.target) (h_vars : Hvar.t list) (v : var) : exp =
  let name = Var.name v in
  let typ = Var.typ v in
  match Hvar.find name h_vars with
  | Some t -> subst_name tgt t name typ
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
  | Some t -> match Hvar.value t with
    | Hvar.Storage {at_entry; _} -> begin
        match at_entry with
        | Hvar.Register name ->
          let lhs = get_reg tgt name in
          Def.create ~tid:(Term.tid ir) lhs rhs
        | Hvar.Memory memory ->
          let mem = get_mem tgt in
          let lhs = mem in
          let endianness =
            let e = Theory.Target.endianness tgt in
            if Theory.Endianness.(e = le) then
              LittleEndian
            else
              BigEndian
          in
          let size = size_of_typ typ name in
          let rhs = match memory with
            | Hvar.Frame (loc, off) ->
              let loc = get_reg tgt loc in
              Bil.
                (store ~mem:(var mem) ~addr:(var loc + int off)
                   rhs endianness size)
            | Hvar.Global addr ->
              Bil.(store ~mem:(var mem) ~addr:(int addr)
                     rhs endianness size) in
          Def.create ~tid:(Term.tid ir) lhs rhs
      end
    | Hvar.Constant const ->
      Def.create ~tid:(Term.tid ir) lhs (Bil.int const)

let subst_label (tgt : Theory.target) (h_vars : Hvar.t list) (label : label) : label KB.t =
  match label with
  | Direct tid ->
    KB.collect Theory.Label.name tid >>| begin function
      | None -> label
      | Some name -> match Hvar.find name h_vars with
        | None -> label
        | Some hvar ->
          let typ = Type.Imm (Theory.Target.code_addr_size tgt) in
          Indirect (subst_name tgt hvar name typ)
    end
  | Indirect _ -> KB.return label

let subst_dsts (tgt : Theory.target) (h_vars : Hvar.t list) (jmp : jmp term) : jmp term KB.t =
  let tid = Term.tid jmp and cond = Jmp.cond jmp in
  match Jmp.kind jmp with
  | Goto label -> subst_label tgt h_vars label >>| Jmp.create_goto ~cond ~tid
  | Call call ->
    Call.target call |> subst_label tgt h_vars >>= fun target ->
    Call.return call |> begin function
      | None -> KB.return @@ Jmp.create_call ~cond ~tid @@ Call.create () ~target
      | Some label -> subst_label tgt h_vars label >>| fun return ->
        Jmp.create_call ~cond ~tid @@ Call.create () ~target ~return
    end
  | Ret label -> subst_label tgt h_vars label >>| Jmp.create_ret ~cond ~tid
  | Int _ -> KB.return jmp

let subst_jmp (tgt : Theory.target) (h_vars : Hvar.t list) (ir : jmp term) : jmp term KB.t =
  Jmp.map_exp ir ~f:(subst_exp tgt h_vars) |> subst_dsts tgt h_vars

let subst_blk (tgt : Theory.target) (h_vars : Hvar.t list) (ir : blk term) : blk term KB.t =
  let+ jmps =
    Term.enum jmp_t ir |>
    Seq.to_list |>
    KB.List.map ~f:(subst_jmp tgt h_vars) in
  let phis = match Term.enum phi_t ir |> Seq.to_list with
    | [] -> []
    | _ -> failwith "subst_blk: Unexpected Phi node!" in
  let defs =
    Term.enum def_t ir |>
    Seq.to_list |>
    List.map ~f:(subst_def tgt h_vars) in
  Blk.create () ~phis ~defs ~jmps ~tid:(Term.tid ir)

let substitute
    (tgt : Theory.target)
    (h_vars : Hvar.t list)
    (ir : blk term list)
  : blk term list KB.t =
  try
    KB.List.map ~f:(subst_blk tgt h_vars) ir
  with
  | Subst_err msg -> err msg
