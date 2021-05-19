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

  let pp fmt t =
    match t with
    | Var v -> Var.pp fmt v
    | Const w -> Word.pp fmt w
    | Operation s -> String.pp fmt s

end

module Pattern_node = struct

  type t =
    | Meta of Var.t (* FIXME: what goes here? *)
    | Concrete of Ast_node.t
  [@@deriving compare, equal]

  let pp fmt t =
    match t with
    | Concrete t -> Ast_node.pp fmt t
    | Meta v -> Format.fprintf fmt "M %a" Var.pp v

  let domain = Knowledge.Domain.optional ~equal:equal "pattern-node-domain"

  let slot = Knowledge.Class.property Theory.Program.cls "pattern-node" domain

  (* Placeholder for Ir.t *)
  type ir = {
    tag : tid;
    mk_ir :
      inputs:(var Var.Map.t) ->
      (* Just one variable for now *)
      outputs:var ->
      (* Just a single operation for now *)
      Ir.operation
  }

  let equal_ir a b = Tid.(a.tag = b.tag)

  let emit_domain = Knowledge.Domain.optional ~equal:equal_ir "ir-domain"

  let emit_slot = Knowledge.Class.property Theory.Program.cls "emit-ir" emit_domain

  type 't match_res =
      Assign of { pat_var : var; tmp_var : var; binding : 't }
    | Trivial
    | Fail

  let match_ (pat : t) (n : Ast_node.t) (binding : 't) : 't match_res =
    match pat with
    | Meta v ->
      let temp = Var.create ~fresh:true "tmp" Unk in
      Assign { pat_var = v; tmp_var = temp; binding = binding }
    | Concrete m ->
      Printf.printf "Matching concrete nodes!\n\n%!";
      if Ast_node.equal m n then Trivial else Fail

  (* The result of a substitution is a map, which assigns pattern
     variables to a pair of a temporary and the term at that leaf, and
     an "emit" list which is a list of instructions to emit. *)
  type ('v, 'a) subst_res = { map : (var * 'v) Var.Map.t; emit : 'a list }

  type ('v, 'a) subst = ('v, 'a) subst_res option

  let init_subst : ('v, 'a) subst = Some { map = Var.Map.empty; emit = [] }

  let add_binding k v m =
    match m with
    | Some { map; emit } ->
      begin
        match Var.Map.add map ~key:k ~data:v with
        | `Duplicate -> None
        | `Ok m -> Some { map = m; emit = emit }
      end
    | None -> None

  let emit e m =
    { m with emit = e::m.emit }

  let add_emit (e : 'a option) (s : ('v, 'a) subst) : ('v, 'a) subst =
    match e, s with
    | Some e, Some s -> Some { s with emit = e::s.emit }
    | _ -> s

  let add_res (res : 'v match_res) (e : 'a option) (s : ('v, 'a) subst) : ('v, 'a) subst =
    match res with
    | Assign { pat_var; tmp_var; binding } -> add_binding pat_var (tmp_var, binding) s
    (* Note: we only emit on a non-variable node *)
    | Trivial -> add_emit e s
    | Fail -> None

  (* Left biaised merge because this doesn't exist in Core_kernel
         apparently *)
  let merge (m1 : ('v, 'a) subst_res) (m2 : ('v, 'a) subst) =
    let res = Var.Map.fold m1.map ~init:m2
        ~f:(fun ~key ~data om ->
            Format.printf "Adding key: %a\n\n%!" Var.pp key;
            add_binding key data om)
    in
    List.fold m1.emit ~init:res
      ~f:(fun om e -> add_emit (Some e) om)

  (* Again: we fail here if there are non-linear patterns *)
  let join_subst (s1 : ('v, 'a) subst) (s2 : ('v, 'a) subst) : ('v, 'a) subst =
    Printf.printf "Calling join_subst\n\n%!";
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
let mk_tagged_app
    (slot : (Theory.program, 'lab option) Knowledge.slot)
    (lab : 'lab)
    ~string_of_lab:(pp_lab : 'lab -> string)
    (args : 'lab ast_raw seq)
    (tag : Pattern_node.ir option)
  : 'lab ast =
  let* node_tid = KB.Object.create Theory.Program.cls in
  let node = AST.Node.create node_tid in
  Format.printf "Creating node: %a: %s\n\n%!" Tid.pp node (pp_lab lab);
  let graph =
    Seq.fold args
      ~init:AST.empty
      ~f:(fun g n ->
          Graphlib.union (module AST) g n.graph)
  in
  let graph = AST.Node.insert node graph in
  let args =
    Seq.map args ~f:(fun n -> n.node)
  in
  let edges =
    Seq.mapi args
      ~f:(fun i n -> AST.Edge.create node n i)
  in
  let graph =
    Seq.fold edges
      ~init:graph
      ~f:(fun g e ->
          Format.printf "Adding edge between %a and %a\n%!"
            Tid.pp (AST.Edge.src e) Tid.pp (AST.Edge.dst e);
          AST.Edge.insert e g)
  in
  let* () = KB.provide slot node (Some lab) in
  let+ () =
    if Option.is_some tag then
      KB.provide Pattern_node.emit_slot node tag
    else
      KB.return ()
  in
  { node; graph }

let mk_app slot lab args = mk_tagged_app slot lab args None

let mk_const slot lab = mk_app slot lab Seq.empty

let mk_meta var = mk_const ~string_of_lab:(fun l -> Format.asprintf "%a" Pattern_node.pp l)
    Pattern_node.slot (Pattern_node.Meta var)

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

let fresh_var = Var.create ~fresh:true "dummy" Unk

let rec match_
    (out_var : var option)
    (pat : Pattern_node.t ast_raw)
    (tm : Ast_node.t ast_raw)
  : (Ast_node.t ast_raw, Ir.operation) Pattern_node.subst KB.t =
  Printf.printf "Calling match_\n\n%!";
  let* pat_hd = get_pat_hd pat in
  let* tm_hd = get_tm_hd tm in
  let pat_children = get_children pat in
  let tm_children = get_children tm in
  let* subst = match_seq pat_children tm_children in
  let+ action = KB.collect Pattern_node.emit_slot pat.node in
  let action =
    Option.(
      subst >>= fun subst ->
      action >>= fun action ->
      out_var >>| fun out_var ->
      let inputs = Var.Map.map subst.map ~f:fst in
      action.Pattern_node.mk_ir
        ~inputs:inputs
        ~outputs:out_var
    )
  in
  Pattern_node.add_res
    (Pattern_node.match_ pat_hd tm_hd tm)
    action
    subst

and match_seq (pats : Pattern_node.t ast_raw list) (tms : Ast_node.t ast_raw list)
  : (Ast_node.t ast_raw, Ir.operation) Pattern_node.subst KB.t =
  let pairs = List.zip_exn pats tms in
  KB.List.fold pairs
    ~init:Pattern_node.init_subst
    ~f:(fun subst (p, t) ->
        (* There is no out variable if we are the child of a node (I think?) *)
        let+ subst_p_t = match_ None p t in
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
  let mk_app = mk_app Ast_node.slot ~string_of_lab:(fun l -> Format.asprintf "%a" Ast_node.pp l) in
  let mk_const = mk_const ~string_of_lab:(fun l -> Format.asprintf "%a" Ast_node.pp l) Ast_node.slot in
  let const_1 = Ast_node.Const (Word.one 32) in
  let const_2 = Ast_node.Const (Word.of_int ~width:32 2) in
  let add = Ast_node.Operation "add" in
  let* arg_1 = mk_const const_1 in
  let* arg_2 = mk_const const_2 in
  let* arg_2' = mk_const const_2 in
  let* sub_term = mk_app add (Seq.of_list [arg_2; arg_2']) in
  mk_app add
    (Seq.of_list [arg_1; sub_term])


(* This could be done with some hidden utilities from Arm_selector *)
let arm_add ~inputs ~outputs =
  let x = Var.Map.find_exn inputs (Var.create "X" (Imm 32)) |> Ir.simple_var in
  let y = Var.Map.find_exn inputs (Var.create "Y" (Imm 32)) |> Ir.simple_var in
  let z = Var.Map.find_exn inputs (Var.create "Z" (Imm 32)) |> Ir.simple_var in
  let out = outputs |> Ir.simple_var in
  let add_op = Ir.Opcode.create ~arch:"arm" "add" in
  Ir.simple_op add_op (Ir.Var out) [Ir.Var x; Ir.Var y; Ir.Var z]


let test_pat =
  let mk_app = mk_tagged_app Pattern_node.slot ~string_of_lab:(fun l -> Format.asprintf "%a" Pattern_node.pp l) in
  let var_x = Var.create "X" (Imm 32) in
  let var_y = Var.create "Y" (Imm 32) in
  let var_z = Var.create "Z" (Imm 32) in
  let add = Pattern_node.Concrete (Ast_node.Operation "add") in
  let* arg_x = mk_meta var_x in
  let* arg_y = mk_meta var_y in
  let* arg_z = mk_meta var_z in
  let* tag = KB.Object.create Theory.Program.cls in
  let* sub_term = mk_app add (Seq.of_list [arg_y; arg_z]) None in
  mk_app add
    (Seq.of_list
       [arg_x; sub_term])
    (Some { tag = tag; mk_ir = arm_add })

let lift_match o p t = KB.Lift.ternary match_ o p t |> KB.join

let test_out = Var.create ~fresh:true "out" Unk

let test_match = lift_match (KB.return @@ Some test_out) test_pat test_tm

let () =
  eval KB.(
      test_tm >>= fun test_tm ->
      test_pat >>= fun test_pat ->
      Graphlib.to_dot (module AST) ~channel:stdout
        ~string_of_node:(fun n -> Tid.to_string n)
        test_tm.graph;
      Printf.printf "\n%!";
      Graphlib.to_dot (module AST) ~channel:stdout
        ~string_of_node:(fun n -> Tid.to_string n)
        test_pat.graph;
      Printf.printf "\n%!";
      test_match >>| fun subst ->
      begin
        if Option.is_none subst then
          Printf.printf "\n\nMatch failed!\n\n"
        else
          begin
            let Pattern_node.{ map; emit } = Option.value_exn subst in
            Printf.printf "\n\nMatch success!\n\n";
            Var.Map.iteri map
              ~f:(fun ~key:v ~data:tmp ->
                  Format.printf "%a |-> %a %a\n" Var.pp v Var.pp (fst tmp) Tid.pp (snd tmp).node);
            Core_kernel.List.iter emit
              ~f:(function o -> Format.printf "%s\n" (Ir.operation_to_string o))
          end
      end)
