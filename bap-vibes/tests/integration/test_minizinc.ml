open !Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_vibes
open OUnit2
module KB = Knowledge
open Knowledge.Syntax

module Ir = Arm_selector.Ir

open Ir

(* Creates an operation built just from the specified variables,
   before they been assigned to operands. *)
let in_op (ts : Var.t list) : operation =
  let tid = Tid.create () in
  { id = tid;
    lhs = List.map ~f:(fun t -> Var (simple_var t)) ts;
    insns = [];
    optional = false;
    operands = [];
  }

(* Creates an operation where the specified variables have been
   assigned to operands. *)
let out_op (ts : Var.t list) : operation =
  let tid = Tid.create () in
  { id = tid;
    lhs = [];
    insns = [];
    optional = false;
    operands = List.map ~f:(fun t -> Var (simple_var t)) ts;
  }

(* Creates an operation with a specified instruction (opcode), a specified
   variable (before it's assigned to operands), and specified operands. *)
let simple_op' opcode arg args =
  let tid = Tid.create () in
  { id = tid;
    lhs = [Var (simple_var arg)];
    insns = [opcode];
    optional = false;
    operands = List.map ~f:(fun t -> Var (simple_var t)) args;
  }

(* Creates a function which takes a set of variables, and uses them to fill
   in the operand slots for an operation with a fixed [lhs] and [opcode]. *)
let (:=) lhs opcode = fun args -> simple_op' opcode lhs args

(* Define some temporaries to use in a test. *)
let t1 = Var.create "t1" (Imm 32)
let t2 = Var.create "t2" (Imm 32)
let t3 = Var.create "t3" (Imm 32)
let temps1 = [t1; t2; t3]

(* Define some opcodes to use in a test. *)
let op1 = (t2 := `MOVi) [t1]
let op2 = (t3 := `MOVi) [t2]
let ops = [op1; op2]

(* Construct a block to use in a test. *)
let blk1 : Ir.blk =
  {
    id = Tid.create ();
    data = ops;
    ctrl = [];
    ins = in_op [t1];
    outs = out_op [t3];
    frequency = 1
  }

(* Map the above temporaries to their `lhs` instantiations. *)
let definer_map1 = Var.Map.of_alist_exn [
    (t1, op_var_exn (List.hd_exn blk1.ins.lhs));
    (t2, op_var_exn (List.hd_exn op1.lhs));
    (t3, op_var_exn (List.hd_exn op2.lhs)) ]

(* Map the above temporaries to their `operands` instantiations. *)
let user_map1 = Var.Map.of_alist_exn [
    (t1, List.map ~f:op_var_exn op1.operands);
    (t2, List.map ~f:op_var_exn op2.operands);
    (t3, List.map ~f:op_var_exn blk1.outs.operands) ]

(* Map the TIDs of the specified operations to their opcodes. *)
let op_insns1 = Tid.Map.of_alist_exn [
    (op1.id, [`MOVi]);
    (op2.id, [`MOVi]);
    ( blk1.ins.id, []);
    (blk1.outs.id, []) ]

(* Use the above to set up some temps, operands, and operations *)
let operands1 = List.map ~f:(fun o -> (op_var_exn o).id)
    (blk1.ins.lhs @ op1.operands @ op1.lhs @ op2.operands @
     op2.lhs @ blk1.outs.operands)
let operations1 = List.map ~f:(fun o -> o.id)
    [blk1.ins; op1; op2; blk1.outs]
let oprnd_temps1 = [ t1; t1; t2; t2; t3; t3 ]

(* Put the above IR into a block we can use for a test. *)
let ex1 : Ir.t = {
  congruent = [];
  blks = [blk1]
}

(* Ensure minizinc produces the expected output on our sample IR block. *)
let test_minizinc_ex1 (ctxt : test_ctxt) : unit =
  let model = Cli.minizinc_model_filepath ctxt in
  let computation =
    (* Set up the KB. *)
    KB.Object.create Data.cls >>= fun obj ->
    Patches.get_bir "ret-3" 32 >>= fun bil ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_bir patch bil >>= fun () ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun () ->
    (* Now run the compiler. *)
    Minizinc.run_minizinc model ex1 >>= fun sol ->
    let get_ops ir = let blk = List.hd_exn ir.blks in
      blk.data in
    assert_bool "Operations should be in order"
      (List.for_all2_exn
         ~f:(fun o1 o2 -> Tid.equal o1.id o2.id)
         (get_ops sol)
         (get_ops ex1));
    KB.return obj
  in
  let _ = KB.run Data.cls computation KB.empty in ()

let suite = [
  "Test Minizinc.run_minizinc" >:: test_minizinc_ex1;
]
