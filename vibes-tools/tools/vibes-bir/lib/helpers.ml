open Core
open Bap.Std
open Bap_core_theory

open KB.Syntax

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
