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

(*******************************************************************
* The VIBES Intermediate Representation.
*
* This module contains the data type and operations that is fed
* into the constraint solver as the last step of the code generation,
* where (currently) register allocation and instruction scheduling are
* performed.
*
* The basic datatype of opcodes ([opcode], or [Opcode.t]) is just a
* fancy string (the BAP [Name.t] type), since that's all the info we
* need for now (though it would be nice to distinguish move from
* non-move operations in the future).
*
* Variables have a unique identifier, and a list of possible virtual
* variables that correspond to it, along with an optional physical
* "preassignment" location.
*
* Operands are a variant type of either:
* - A variable (probably the most common case)
* - A bitvector constant
* - A label (for jumps)
* - A "void" operand, representing a name which will disappear on
*   printing, or a "virtual" resource (e.g. memory, or a "branch taken"
*   tag)
* - An offset (for concrete locations)
*
* It's a bit annoying to distinguish the two last ones, but it turns out
* to be quite useful, if we're creating branching structure in our
* patch, to not have to do crazy mathematics to determine the location
* of the target. The offsets are necessary if we need to jump somewhere
* outside of the patch code (which is quite common).
*
* Void operands are necessary for instruction scheduling: basically a
* void operand should be assigned and used by operations whose order
* cannot be swapped: e.g. in the sequence [str foo bar; ld ri boz],
* [str] should assign to a void [mem] variable, and [ld] should read
* from that same variable, so that their order is preserved (unless you
* can prove that the locations do not alias).
*
* Similar issues can arise with branching, though that typically occurs
* at the end of a block and is not reordered.
*
*
* Registers are allocated based on their role: only certain registers
* are available to certain roles in the minizinc constraint environment.
*
* Because we don't want to assign *actual* resources to the void
* locations, we tag all the variables with a "dummy" role, to
* avoid using up precious real locations.
*
************************************************************************)
open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

type opcode = Knowledge.Name.t [@@deriving compare, equal, sexp]

let dummy_role = Theory.Role.declare ~package:"vibes" "dummy"

let preassigned = Theory.Role.declare ~package:"vibes" "preassigned"

module Opcode =
struct

  type t = opcode

  let create ?arch:arch s = Knowledge.Name.create ?package:arch s

  let name o = Knowledge.Name.unqualified o

  let (=) = equal_opcode

  module Set : (Set.S with type Elt.t = opcode)
    = Set.Make(Knowledge.Name)

  module Map : (Map.S with type Key.t = opcode)
    = Map.Make(Knowledge.Name)

end

type op_var = {
  id : Var.t;
  temps : Var.t list;
  pre_assign : var option
} [@@deriving compare, sexp]

let equal_op_var x y = [%equal: Var.t] x.id y.id

let simple_var v =
  {
    id = Var.create ~fresh:true "operand" (Var.typ v);
    temps = [v];
    pre_assign = None
  }

let given_var v ~reg:reg =
  {
   id = Var.create ~fresh:true "operand" (Var.typ v);
    temps = [v];
    pre_assign = Some reg
  }

type operand =
  | Var of op_var
  | Const of Word.t
  | Label of Tid.t
  | Void of op_var
  | Offset of Word.t [@@deriving compare, equal, sexp]


let freshen_operand o =
  match o with
  | Var v ->
    let fresh_v =
      { v with
        id =
          Var.create
            ~is_virtual:true
            ~fresh:true
            (Var.name v.id)
            (Var.typ v.id)
      } in
    Var fresh_v
  | Void v ->
    let fresh_v =
      { v with
        id =
          Var.create
            ~is_virtual:true
            ~fresh:true
            (Var.name v.id)
            (Var.typ v.id)
      } in
    Void fresh_v
  | _ -> o

let op_var_exn (x : operand) : op_var =
  match x with
  | Var o -> o
  | _ -> failwith "Expected op_var"

let create_id =
  let next = ref 0 in
  fun () ->
    let id = !next in
    incr next;
    id

type operation = {
  id : int;
  lhs : operand list;
  opcodes : opcode list;
  optional : bool;
  operands :  operand list;
} [@@deriving compare, equal, sexp]

let simple_op opcode arg args =
  { id = create_id ();
    lhs = [arg];
    opcodes = [opcode];
    optional = false;
    (* Operands need to have unique ids *)
    operands = List.map ~f:freshen_operand args;
  }

let empty_op () : operation =
  { id = create_id ();
    lhs = [];
    opcodes = [];
    optional = false;
    operands = []
  }

let write_multiple_op opcode written args =
  { id = create_id ();
    lhs = written;
    opcodes = [opcode];
    optional = false;
    (* Operands need to have unique ids *)
    operands = List.map ~f:freshen_operand args;
  } 

let op_no_args opcode =
  { id = create_id ();
    lhs = [];
    opcodes = [opcode];
    optional = false;
    operands = [];
  }

let mk_empty_operation () =
  { id = create_id ();
    lhs = [];
    opcodes = [];
    optional = false;
    operands = [];
  }

type blk = {
  id : Tid.t;
  data : operation list;
  ctrl : operation list;
  ins : operation;
  outs : operation;
  frequency : int
} [@@deriving compare, equal, sexp]


let simple_blk tid ~data ~ctrl =
  {
    id = tid;
    data= data;
    ctrl = ctrl;
    (* Probably we should just add every variable in ops here *)
    ins = mk_empty_operation ();
    outs = mk_empty_operation ();
    frequency = 1
  }


type t = {
  blks : blk list;
  congruent : (op_var * op_var) list
} [@@deriving compare, equal, sexp]

let empty = {blks = []; congruent = []}

(* Preserve the ordering when deduping. This is much slower than `dedup_and_sort`. *)
let dedup_list_stable l ~compare =
  let equal x x' = compare x x' = 0 in
  let rec loop res = function
    | [] -> res
    | x :: xs ->
      let dups = List.find_all_dups (x :: xs) ~compare in
      let res = if List.mem dups x ~equal then res else x :: res in
      loop res xs
  in
  loop [] (List.rev l)

let union t1 t2 =
  let comp_pair = Tuple.T2.compare ~cmp1:compare_op_var ~cmp2:compare_op_var in
  {
    blks =
      dedup_list_stable ~compare:compare_blk (t1.blks @ t2.blks);
    congruent =
      dedup_list_stable ~compare:comp_pair (t1.congruent @ t2.congruent)
  }

let add blk t =
  {t with blks = blk::t.blks}


let operation_to_string (o : operation) = Int.to_string o.id
let op_var_to_string (o : op_var) = Var.to_string o.id

let var_operands (ops : operand list) : op_var list =
  List.fold ~f:(fun acc o ->
      match o with
      | Var v | Void v -> v :: acc
      | Const _ | Label _ | Offset _ -> acc
    ) ~init:[] ops

module Blk = struct

  let all_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.lhs @ operation.operands)
    in
    blk.ins.lhs @
    blk.outs.operands @
    (operation_operands blk.data) @
    (operation_operands blk.ctrl)

  let all_rhs_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.operands)
    in
    blk.ins.operands @
    blk.outs.operands @
    (operation_operands blk.data) @
    (operation_operands blk.ctrl)

  let all_lhs_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.lhs)
    in
    blk.ins.lhs @ blk.outs.lhs @ (operation_operands blk.data) @ (operation_operands blk.ctrl)

  let all_temps (blk : blk) : Var.Set.t =
    List.concat_map (all_operands blk)
      ~f:(fun op ->
          match op with
          | Const _ | Label _ | Offset _ -> []
          | Var op | Void op -> op.temps) |>
    Var.Set.of_list

  let definer_map (blk : blk) : op_var Var.Map.t =
    List.fold
      (all_lhs_operands blk |> var_operands)
      ~init:Var.Map.empty
      ~f:(fun acc operand ->
          List.fold operand.temps
            ~init:acc
            ~f:(fun acc tmp ->
                Var.Map.add_exn acc ~key:tmp ~data:operand))

  let users_map (blk : blk) : (op_var list) Var.Map.t =
    List.fold (all_rhs_operands blk |> var_operands)
      ~init:Var.Map.empty
      ~f:(fun acc operand ->
          List.fold operand.temps ~init:acc ~f:(fun acc temp ->
              Var.Map.update acc temp
                ~f:(fun mrandlist ->
                    match mrandlist with
                    | Some operandlist -> operand :: operandlist
                    | None -> [operand])
            )
        )

  let all_operations (blk : blk) : operation list =
    blk.ins :: ( blk.outs :: (blk.data @ blk.ctrl))

  let operation_opcode (blk : blk) : (opcode list) Int.Map.t =
    List.fold ~init:Int.Map.empty ~f:(fun acc o ->
        Int.Map.add_exn acc ~key:o.id ~data:o.opcodes
      ) (all_operations blk)

  let operand_operation (blk : blk) : operation Var.Map.t =
    let alist =
      List.concat_map (all_operations blk)
        ~f:(fun operation ->
            let rhs_operands =
              List.map (var_operands operation.operands)
                ~f:(fun op -> (op.id, operation))
            in
            let lhs_operands =
              List.map (var_operands operation.lhs)
                ~f:(fun op -> (op.id, operation)) in
            lhs_operands @ rhs_operands)
    in
    Var.Map.of_alist_exn alist

end


let var_map_union_exn m1 m2 =
  Var.Map.merge m1 m2
    ~f:(fun ~key:_ ab ->
        match ab with
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both(_ , _) -> failwith "Map has both keys")

let int_map_union_exn m1 m2 =
  Int.Map.merge m1 m2
    ~f:(fun ~key:_ ab ->
        match ab with
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both(_ , _) -> failwith "Map has both keys")

let map_blks ~f (sub : t) : t =
  {blks = List.map ~f sub.blks; congruent = sub.congruent}

let map_operations ~f (vir : t) : t =
  map_blks vir
    ~f:(fun b ->
        { id = b.id;
          data = List.map ~f b.data;
          ctrl = List.map ~f b.ctrl;
          ins = f b.ins;
          outs = f b.outs;
          frequency = b.frequency })

let map_op_vars ~f (vir : t) : t =
  let f2 o = match o with
    | Var o -> Var (f o)
    | Void o -> Void (f o)
    | o -> o
  in
  let apply_to_op (o : operation) : operation =
    {
      o with
      lhs = List.map ~f:f2 o.lhs;
      operands = List.map ~f:f2 o.operands;
    }
  in
  {
    blks =
      List.map vir.blks
        ~f:(fun b ->
            {
              b with
              data = List.map ~f:apply_to_op b.data;
              ctrl = List.map ~f:apply_to_op b.ctrl;
              ins = apply_to_op b.ins;
              outs = apply_to_op b.outs;
            }
          );
    congruent = List.map ~f:(Tuple2.map ~f:f) vir.congruent;
  }

let all_temps (sub : t) : Var.Set.t =
  List.fold sub.blks
    ~init:Var.Set.empty
    ~f:(fun acc blk ->
        Var.Set.union acc (Blk.all_temps blk))

let all_operands (sub : t) : Var.Set.t =
  List.concat_map sub.blks ~f:Blk.all_operands |>
  var_operands |>
  List.map ~f:(fun (o : op_var) -> o.id) |>
  Var.Set.of_list

let preassign_map (sub : t) : (var option) Var.Map.t =
  List.concat_map sub.blks ~f:Blk.all_operands |>
  var_operands |>
  List.map ~f:(fun op -> (op.id, op.pre_assign))
  |> Var.Map.of_alist_exn


let definer_map (sub : t) : op_var Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        var_map_union_exn acc (Blk.definer_map blk))


let users_map (sub : t) : (op_var list) Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        let m = Blk.users_map blk in
        Var.Map.merge acc m
          ~f:(fun ~key:_ ab ->
              match ab with
              | `Left a -> Some a
              | `Right b -> Some b
              | `Both(a,b) -> Some (a @ b)))

let temp_blk (sub : t) : Tid.t Var.Map.t =
  List.concat_map sub.blks
    ~f:(fun blk ->
        List.map (Blk.all_temps blk |> Var.Set.to_list)
          ~f:(fun t -> (t, blk.id))) |>
  Var.Map.of_alist_exn

let operation_opcodes (sub : t) : (opcode list) Int.Map.t =
  List.fold sub.blks
    ~init:Int.Map.empty
    ~f:(fun acc blk ->
        int_map_union_exn acc (Blk.operation_opcode blk))

let operand_operation (sub : t) : operation Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        var_map_union_exn acc (Blk.operand_operation blk))

let pretty_operand o =
  match o with
  | Var o ->
    sprintf "%s : %s < %s"
      (Var.to_string o.id)
      (List.map ~f:Var.to_string o.temps |> String.concat ~sep:"::")
      ((Option.map
          ~f:(Var.to_string) o.pre_assign) |>
       Option.value ~default:"N/A")
  | Void o ->
    sprintf "(void) %s : %s < %s"
      (Var.to_string o.id)
      (List.map ~f:Var.to_string o.temps |> String.concat ~sep:"::")
      ((Option.map
          ~f:(Var.to_string) o.pre_assign) |>
       Option.value ~default:"N/A")
  | Const c -> Word.to_string c
  | Label l -> Tid.to_string l
  | Offset c -> Format.asprintf "Offset(%d)" (Word.to_int_exn c)

let pretty_operand_list l =
  List.map ~f:pretty_operand l |> String.concat ~sep:","

let pretty_operation (o : operation) =
  sprintf "\t\t%s: [%s]  <- %s [%s]"
    (Int.to_string o.id)
    (pretty_operand_list o.lhs)
    (String.concat
       (List.map o.opcodes ~f:Opcode.name))
    (pretty_operand_list o.operands)

let pretty_blk b = sprintf "blk : %s \n\tins : %s \n\touts: %s\n\tdata: \n%s\n\tctrl: \n%s"
    (Tid.to_string b.id)
    (pretty_operation b.ins)
    (pretty_operation b.outs)
    (List.fold b.data ~init:""
       ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))
    (List.fold b.ctrl ~init:""
       ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))

let pretty_ir (vir : t) : string =
  List.fold vir.blks ~init:"" ~f:(fun acc b -> acc ^ (pretty_blk b) ^ "\n\n")

let to_string t = pretty_ir t

let dummy_reg_alloc t =
  map_op_vars t
    ~f:(fun v ->
        match v.pre_assign with
        | Some _ -> v
        | None ->
          let var = List.hd_exn v.temps in
          {v with pre_assign = Some (Var.create "R0" (Var.typ var))})

module Operand =
struct
  type t = operand
  let compare = compare_operand
  let sexp_of_t = sexp_of_operand
  let t_of_sexp = operand_of_sexp
end

module OpSet = Set.Make(Operand)

let is_defined (defs : OpSet.t) (v : operand) : bool =
  let vars =
    match v with
    | Var v | Void v -> v.temps
    | _ -> []
  in
  OpSet.fold defs ~init:false
    ~f:(fun is_def o ->
        match o with
        | Var v | Void v ->
          let def_vars = v.temps in
          let v_set = Var.Set.of_list vars in
          let def_set = Var.Set.of_list def_vars in
          let b = Var.Set.is_empty (Var.Set.inter v_set def_set) in
          (not b) || is_def
        | _ -> is_def)

let add_in_vars_blk (b : blk) : blk =
  let ops = b.data @ b.ctrl in
  (* let collect_vars_op_list l =
    List.fold l ~init:OpSet.empty
      ~f:(fun set o ->
          match o with
          | Var _ | Void _ -> OpSet.add set o
          | _ -> set)
  in *)
  (* Simultaneously collect defined and undefined vars
  This can't possibly be right, because the order of operations is not trustworthy in vibes ir
  *)
  (* 
  let _, undefined =
    List.fold ops ~init:(OpSet.empty, OpSet.empty)
      ~f:(fun (defined, undefined) o ->
          let undef = collect_vars_op_list o.operands in
          let undef = OpSet.filter undef
              ~f:(fun o -> not @@ is_defined defined o)
          in
          let undef = OpSet.filter undef
              ~f:(fun o -> not @@ is_defined undefined o)
          in
          let def = collect_vars_op_list o.lhs in
          OpSet.union defined def, OpSet.union undefined undef)
  in *)
  let collect_temps (l : operand list) : Var.Set.t =
      List.fold l ~init:Var.Set.empty
        ~f:(fun set o ->
            match o with
            | Var op_var | Void op_var -> Var.Set.union set (Var.Set.of_list op_var.temps)
            | _ -> set)
    in
  let lhs_temps, rhs_temps =
  List.fold ops ~init:(Var.Set.empty, Var.Set.empty)
    ~f:(fun (lhs_temps, rhs_temps) o ->
        let rhs = collect_temps o.operands in
        let lhs = collect_temps o.lhs in
        Var.Set.union lhs lhs_temps, Var.Set.union rhs rhs_temps)
  in
  let undef_temps = Var.Set.diff rhs_temps lhs_temps |> Var.Set.to_list in
  (* Ok. No dice. I need Voids to stay void and Vars to stay vars *)
  let ins = List.map undef_temps ~f:(fun temp -> Var (simple_var temp))
  in
  (* We add dummy operation with no instructions and as lhs all the
     [ins] variables *)
  let ins = {
    id = create_id ();
    lhs = ins;
    opcodes = [];
    optional = false;
    operands = [];
  } in
  {b with ins = ins}

(* Collect all the variables appearing on rhs that are not defined by
   an lhs before-hand, and add them to the [in] field. *)
let add_in_vars (t : t) : t =
  { t with blks = List.map ~f:add_in_vars_blk t.blks }


(* Get every single opcode, removing duplicates. *)
let all_opcodes (t : t) : opcode list =
  let blks = t.blks in
  let op_set = List.fold blks ~init:Opcode.Set.empty
      ~f:(fun acc b ->
          let ops = b.ins :: b.outs :: b.data @ b.ctrl in
          List.fold ops ~init:acc
            ~f:(fun acc o ->
                List.fold o.opcodes ~init:acc
                  ~f:(fun acc code ->
                      Opcode.Set.add acc code)
              )
        )
  in
  Opcode.Set.to_list op_set


(* Iterate through the operations, and assign roles to all the
   operands based on the opcode.

   In theory we could be architecture + operation specific, e.g. a
   stack load operation vs a mov.

   In practice though, we assign
   - [Theory.Role.Register.general] to all non [Void] variables
   - [dummy_role] to [Void] vars
   - [preassigned] to preassigned vars

*)
let op_classes (t : t) : (Theory.role Opcode.Map.t) Var.Map.t =
  let blks = t.blks in
  let opcodes = all_opcodes t in
  let op_classes_op acc (o : operation) =
    let vars = o.lhs @ o.operands in
    (* for a given var map, variable and role, add to acc the binding
       v |-> (o |-> role) for every o in opcodes. *)
    let map_of_role acc (v : var) role =
      let alist = List.map opcodes
          ~f:(fun o -> (o, role))
      in
      let omap = Opcode.Map.of_alist_exn alist in
      Var.Map.set acc ~key:v ~data:omap
    in
    List.fold vars ~init:acc
      ~f:(fun acc v ->
          match v with
          | Var v ->
            begin
              match v.pre_assign with
              | None -> map_of_role acc v.id Theory.Role.Register.general
              | Some _ -> map_of_role acc v.id preassigned
            end
          | Void v -> map_of_role acc v.id dummy_role
          | _ -> acc
        )
  in
  let op_classes_blk acc (b : blk) =
    let ops = b.ins :: b.outs :: b.data @ b.ctrl in
    List.fold ops ~init:acc ~f:op_classes_op
  in
  List.fold blks ~init:Var.Map.empty ~f:op_classes_blk


let map_operands ~f vir =
  map_operations vir
  ~f:(fun operation ->
    {
      operation with
      operands = List.map ~f operation.operands;
      lhs = List.map ~f operation.lhs;
    }
    )
let freshen_operands vir =
  map_operands ~f:freshen_operand vir

let freshen_operation_ids vir =
  map_operations vir ~f:(fun operation ->
    {
      operation with
      id = create_id ()
    })
