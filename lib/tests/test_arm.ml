open !Core_kernel
open Bap_knowledge
open Bap_vibes
open Bap_core_theory

module KB = Knowledge

open KB.Let
module H = Helpers

open OUnit2

open Theory

module Prog1 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let v =
    let r0 = Var.define (Bitv.define 32) "r0" in
    let r1 = Var.define (Bitv.define 32) "r1" in
    let r2 = Var.define (Bitv.define 32) "r2" in
    let data = data_body [r0 := var r1 + var r2] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.create () in
    blk l data control

end


module Prog2 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let v =
    let r0 = Var.define (Bitv.define 32) "r0" in
    let r1 = Var.define (Bitv.define 32) "r1" in
    let r2 = Var.define (Bitv.define 32) "r2" in
    let data = data_body [r0 := var r1 + (var r2 + var r0)] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.create () in
    blk l data control

end

module Prog1_inst = Prog1(Arm_gen.ARM_Core)

module Prog2_inst = Prog2(Arm_gen.ARM_Core)

let test_ir (_ : test_ctxt) (v : unit eff) (expected : string list) : unit =
  let computation =
    let* obj = H.obj () in
    let* v = v in
    let v = Arm_gen.effect v in
    let* _ =
      v |> Option.map ~f:Arm_gen.ir
      |> Option.map ~f:Arm_gen.arm_ir_pretty
      |> Option.map ~f:Result.ok (* We turn an [Error foo] into a [None] *)
      |> Option.join (* And we squash the Options *)
      |> Data.Patch.set_assembly obj
    in
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in
  let expected = Some expected in
  H.assert_property
    ~cmp:(Option.equal (List.equal String.equal))
    ~printer:H.print_string_list_opt
    Data.Patch.assembly expected result

let test_ir1 ctxt =
  test_ir ctxt Prog1_inst.v ["mov r0, #1"; "add #1, r1, r2"]

let test_ir2 ctxt =
  test_ir ctxt Prog2_inst.v ["mov r0, #2"; "add #2, r1, #1"; "add #1, r2, r0"]

let suite =
  [
    "Test Arm_gen.ir 1" >:: test_ir1;
    "Test Arm_gen.ir 2" >:: test_ir2;
  ]
