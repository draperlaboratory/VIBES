open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Graphlib.Std

open KB.Let

module Ast_node = struct

  type t =
    | Var of Var.t
    | Const of Word.t
    | Operation of string
  [@@deriving compare, equal]

  let domain = Knowledge.Domain.optional ~equal:equal "ast-node-domain"

  let slot = Knowledge.Class.property Theory.Program.cls "ast-node" domain

end

module Pattern_node = struct

  type t =
    | Meta of Var.t (* FIXME: what goes here? *)
    | Concrete of Ast_node.t
  [@@deriving compare, equal]

  let domain = Knowledge.Domain.optional ~equal:equal "pattern-node-domain"

  let slot = Knowledge.Class.property Theory.Program.cls "pattern-node" domain

  type 't match_res = Assign of var * 't | Trivial | Fail

  let match_ (pat : t) (n : Ast_node.t) (tm : 't) : 't match_res =
      match pat with
      | Meta v -> Assign (v, tm)
      | Concrete m ->
        if Ast_node.equal m n then Trivial else Fail

  type 't subst = 't Var.Map.t option

  let init_subst : 't subst = Some Var.Map.empty

  let add_option k v m =
    match Var.Map.add m ~key:k ~data:v with
    | `Duplicate -> None
    | `Ok m -> Some m

  let add_res (res : 't match_res) (s : 't subst) : 't subst =
    match res, s with
    | Assign (v, t), Some m -> add_option v t m
    | Trivial, om -> om
    | _ -> None

  (* Left biaised merge because this doesn't exist in Core_kernel
         apparently *)
  let merge m1 m2 =
    Var.Map.fold m1 ~init:m2
      ~f:(fun ~key ~data om -> Option.bind om ~f:(add_option key data))

  (* Again: we fail here if there are non-linear patterns *)
  let join_subst (s1 : 't subst) (s2 : 't subst) : 't subst =
    match s1, s2 with
    | Some m1, m2 -> merge m1 m2
    | _ -> None

end

module AST = Graphlib.Make(Tid)(Int)

(* The type of ASTs: invariant: every "child" of the current node
   should be present in the graph. *)
type 'lab ast_raw = { graph : AST.t; node : AST.node }
type 'lab ast = 'lab ast_raw KB.t


(* Creates a fresh AST node with prescribed label and children *)
let mk_app
    (slot : (Theory.program, 'lab option) Knowledge.slot)
    (lab : 'lab)
    (args : 'lab ast seq)
  : 'lab ast =
  let* node_tid = KB.Object.create Theory.Program.cls in
  let node = AST.Node.create node_tid in
  let* graph =
    KB.Seq.fold args
      ~init:AST.empty
      ~f:(fun g n ->
          let+ n = n in
          Graphlib.union (module AST) g n.graph)
  in
  let* args =
    KB.Seq.map args ~f:(fun n -> let+ n = n in n.node)
  in
  let edges =
    Seq.mapi args
      ~f:(fun i n -> AST.Edge.create node n i)
  in
  let graph = AST.Node.insert node graph in
  let graph =
    Seq.fold edges
      ~init:graph
      ~f:(fun g e -> AST.Edge.insert e g)
  in
  let+ () = KB.provide slot node (Some lab) in
  { node; graph }


let mk_const slot lab = mk_app slot lab Seq.empty

let get_pat_hd p =
  let+ p = KB.collect Pattern_node.slot (AST.Node.label p.node) in
  Option.value_exn p

let get_tm_hd t =
  let+ t = KB.collect Ast_node.slot (AST.Node.label t.node) in
  Option.value_exn t

let get_children t =
  let c = AST.Node.outputs t.node t.graph |> Seq.to_list in
  let c = List.sort c
      ~compare:(fun e1 e2 -> Int.compare (AST.Edge.label e1) (AST.Edge.label e2))
  in
  List.map c
    ~f:(fun e ->
        let tid = AST.Edge.dst e in
        { node = tid; graph = t.graph })

let rec match_ (pat : Pattern_node.t ast_raw) (tm : Ast_node.t ast_raw)
  : (Ast_node.t ast_raw) Pattern_node.subst KB.t =
  let* pat_hd = get_pat_hd pat in
  let* tm_hd = get_tm_hd tm in
  let pat_children = get_children pat in
  let tm_children = get_children tm in
  let+ subst = match_seq pat_children tm_children in
  Pattern_node.add_res
    (Pattern_node.match_ pat_hd tm_hd tm)
    subst

and match_seq (pats : Pattern_node.t ast_raw list) (tms : Ast_node.t ast_raw list)
  : (Ast_node.t ast_raw) Pattern_node.subst KB.t =
  let pairs = List.zip_exn pats tms in
  KB.List.fold pairs
    ~init:Pattern_node.init_subst
    ~f:(fun subst (p, t) ->
        let+ subst_p_t = match_ p t in
        Pattern_node.join_subst subst_p_t subst)


(* Is this legit? *)
let eval (m : 'a KB.t) : unit =
  let open KB.Let_syntax in
  let state = KB.empty in
  let m = m >>= fun _ -> KB.Object.create Theory.Program.cls in
  let _ : _ = KB.run
      Theory.Program.cls
      m
      state
  in
  ()

let test_tm =
  let mk_app = mk_app Ast_node.slot in
  let mk_const = mk_const Ast_node.slot in
  let const_1 = Ast_node.Const (Word.one 32) in
  let const_2 = Ast_node.Const (Word.of_int ~width:32 2) in
  let add = Ast_node.Operation "add" in
  let arg_1 = mk_const const_1 in
  let arg_2 = mk_const const_2 in
  mk_app add (Seq.of_list [arg_1; arg_2])

let test_pat =
  let mk_app = mk_app Pattern_node.slot in
  let mk_const = mk_const Pattern_node.slot in
  let var_1 = Pattern_node.Meta (Var.create "X" (Imm 32)) in
  let var_2 = Pattern_node.Meta (Var.create "Y" (Imm 32)) in
  let add = Pattern_node.Concrete (Ast_node.Operation "add") in
  let arg_1 = mk_const var_1 in
  let arg_2 = mk_const var_2 in
  mk_app add (Seq.of_list [arg_1; arg_2])

let lift_match p t = KB.Lift.binary match_ p t |> KB.join

let test_match = lift_match test_pat test_tm

let () =
  eval KB.(
      test_match >>| fun subst ->
      begin
        if Option.is_none subst then
          Printf.printf "\n\nMatch failed!\n\n"
        else
          begin
          Printf.printf "\n\nMatch success!\n\n";
          Var.Map.iter_keys (Option.value_exn subst)
            ~f:(fun v -> Format.printf "%a\n" Var.pp v)
        end
      end)
