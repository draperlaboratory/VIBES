open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_core_theory

module KB = Knowledge

open KB.Let
module H = Helpers

open OUnit2

open Theory

module Prog (S : Core) = struct

  open S

  let v =
    let r0 = Var.define (Bitv.define 32) "r0" in
    let r1 = Var.define (Bitv.define 32) "r1" in
    let r2 = Var.define (Bitv.define 32) "r2" in
    let data = set r0 (add (var r1) (var r2)) in
    let control = perform (Effect.Sort.fall) in
    let l = Bap.Std.Tid.create () in
    blk l data control

end

module Prog_inst = Prog(Arm_gen.ARM_Core)

let test_ir (ctxt : test_ctxt) : unit =
  let computation =
    let* obj = H.obj () in
    let* v = Prog_inst.v in
    let v = Arm_gen.effect v in
    let* _ =
      v |> Option.map ~f:Arm_gen.ir
      |> Option.map ~f:Arm_gen.arm_ir_pretty
      |> Data.Patch.set_assembly obj
    in
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in
  let expected = Some ["mov r0, #1"; "add #1, r1, r2"] in
  H.assert_property
    ~cmp:(Option.equal (List.equal String.equal))
    ~printer:H.print_string_list_opt
    Data.Patch.assembly expected result

let suite =
  [
    "Test Arm_gen.ir" >:: test_ir;
  ]
