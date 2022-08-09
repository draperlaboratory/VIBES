open Core
open Bap.Std
open Bap_core_theory

open KB.Syntax

module Attr = Vibes_constants.Attr

let spill_tag : unit tag = Value.Tag.register (module Unit)
    ~name:(Attr.make "spilled")
    ~uuid:"a770b736-07d1-11ed-84a7-7f2a318d4806"

let argument_tag : unit tag = Value.Tag.register (module Unit)
    ~name:(Attr.make "argument")
    ~uuid:"2ecbeae4-a576-406c-b95c-324083406c85"

module Var_set = struct

  include Var.Set

  let pp (ppf : Format.formatter) (s : t) : unit =
    Format.fprintf ppf "%s" @@
    List.to_string ~f:Var.to_string @@
    Set.to_list s

end

module Var_map = struct

  type t = Var.Set.t Var.Map.t
  [@@deriving bin_io, sexp, compare]

  let pp (ppf : Format.formatter) (m : t) : unit =
    Format.fprintf ppf "%s" @@
    List.to_string ~f:(fun (x, y) ->
        Format.sprintf "%s %s" (Var.to_string x)
          (List.to_string ~f:Var.to_string @@ Set.to_list y)) @@
    Map.to_alist m

end

let ins_tag : Var.Set.t tag = Value.Tag.register (module Var_set)
    ~name:(Attr.make "ins")
    ~uuid:"f5274885-8155-427c-8d9f-7efebaf00827"

let outs_tag : Var.Set.t tag = Value.Tag.register (module Var_set)
    ~name:(Attr.make "outs")
    ~uuid:"dd9a51b0-bfed-41e3-82a7-eba42a242428"

let congruences_tag : Var.Set.t Var.Map.t tag =
  Value.Tag.register (module Var_map)
    ~name:(Attr.make "congruences")
    ~uuid:"62fc18a7-61ff-4ee3-b02d-9e4a6b18cea9"

let create_sub
    (name : string)
    (blks : blk term list) : sub term KB.t =
  if List.is_empty blks then
    KB.fail @@ Errors.No_blks
      "Vibes_bir.Helpers.create_sub: got an empty list of blks"
  else
    let+ tid = Theory.Label.fresh in
    Sub.create ~name ~blks ~tid ()

let no_jmps (blk : blk term) : bool =
  Seq.is_empty @@ Term.enum jmp_t blk

let is_unconditional (jmp : jmp term) : bool =
  match Jmp.cond jmp with
  | Int _ -> true
  | _ -> false

let is_call (jmp : jmp term) : bool = match Jmp.kind jmp with
  | Call _ -> true
  | _ -> false

let is_indirect_label : label -> bool = function
  | Indirect _ -> true
  | Direct _ -> false

let is_exit_call (jmp : jmp term) : bool = match Jmp.kind jmp with
  | Call call ->
    Call.return call |>
    Option.value_map ~default:true ~f:is_indirect_label
  | _ -> false

let exit_blks (sub : sub term) : (blk term list, KB.conflict) result =
  match Term.enum blk_t sub |> Seq.to_list with
  | [] ->
    let msg = "Vibes_bir.Helpers.exit_blks: got an empty list of blks" in
    Error (Errors.No_blks msg)
  | [_] as blks -> Ok blks
  | _ ->
    let cfg = Sub.to_cfg sub in
    let nodes_seq = Graphs.Ir.nodes cfg in
    let nodes = Seq.to_list nodes_seq in
    Result.return @@ List.filter_map nodes ~f:(fun node ->
        let blk = Graphs.Ir.Node.label node in
        if Graphs.Ir.Node.degree node cfg ~dir:`Out = 0 then Some blk
        else match Term.enum jmp_t blk |> Seq.to_list with
          | [] -> Some blk
          | [jmp] when not @@ is_unconditional jmp -> Some blk
          | jmps -> Option.some_if (List.exists jmps ~f:is_exit_call) blk)

let has_call (blk : blk term) : bool =
  Term.enum jmp_t blk |> Seq.exists ~f:is_call

let call_blks (sub : sub term) : blk term list =
  Term.enum blk_t sub |> Seq.to_list |>
  List.filter_map ~f:(fun blk -> Option.some_if (has_call blk) blk)

let entry_blk : sub term -> (blk term, KB.conflict) result =
  let error =
    Errors.No_blks "Vibes_bir.Helpers.entry_blk: got an empty list of blks" in
  fun sub -> Term.first blk_t sub |> Result.of_option ~error

let entry_tid (sub : sub term) : (tid, KB.conflict) result =
  entry_blk sub |> Result.map ~f:Term.tid
