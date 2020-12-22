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

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 + var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end


module Prog2 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 + (var v3 + var v1)] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog3 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 << var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control


end

module Prog4 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 >> var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog5 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 & var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog6 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [v1 := var v2 |$ var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog7 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define Bool.t "v1" in
    let v2 = Var.define Bool.t "v2" in
    let v3 = Var.define Bool.t "v3" in
    let data = data_body [v1 := var v2 && var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog8 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let v1 = Var.define Bool.t "v1" in
    let v2 = Var.define Bool.t "v2" in
    let v3 = Var.define Bool.t "v3" in
    let data = data_body [v1 := var v2 || var v3] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog9 (S : Core) = struct

  open S
  open Core_notations.Make(S)

  let prog =
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let data = data_body [] in
    let control = ctrl_body [goto tgt] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control

end

module Prog1_inst = Prog1(Arm_gen.ARM_Core)

module Prog2_inst = Prog2(Arm_gen.ARM_Core)

module Prog3_inst = Prog3(Arm_gen.ARM_Core)

module Prog4_inst = Prog4(Arm_gen.ARM_Core)

module Prog5_inst = Prog5(Arm_gen.ARM_Core)

module Prog6_inst = Prog6(Arm_gen.ARM_Core)

module Prog7_inst = Prog7(Arm_gen.ARM_Core)

module Prog8_inst = Prog8(Arm_gen.ARM_Core)

module Prog9_inst = Prog9(Arm_gen.ARM_Core)

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
  test_ir ctxt Prog1_inst.prog ["@entry:"; "add #1, v2, v3"; "mov v1, #1"]

let test_ir2 ctxt =
  test_ir ctxt Prog2_inst.prog ["@entry:"; "add #1, v3, v1"; "add #2, v2, #1"; "mov v1, #2"]

let test_ir3 ctxt =
  test_ir ctxt Prog3_inst.prog ["@entry:"; "lsl #1, v2, v3"; "mov v1, #1"]

let test_ir4 ctxt =
  test_ir ctxt Prog4_inst.prog ["@entry:"; "lsr #1, v2, v3"; "mov v1, #1"]

let test_ir5 ctxt =
  test_ir ctxt Prog5_inst.prog ["@entry:"; "and #1, v2, v3"; "mov v1, #1"]

let test_ir6 ctxt =
  test_ir ctxt Prog6_inst.prog ["@entry:"; "orr #1, v2, v3"; "mov v1, #1"]

let test_ir7 ctxt =
  test_ir ctxt Prog7_inst.prog ["@entry:"; "and #1, v2, v3"; "mov v1, #1"]

let test_ir8 ctxt =
  test_ir ctxt Prog8_inst.prog ["@entry:"; "orr #2, v2, v3"; "mov v1, #2"]

let test_ir9 ctxt =
  test_ir ctxt Prog9_inst.prog ["@entry:"; "bx @tgt"]

let suite =
  [
    "Test Arm_gen.ir 1" >:: test_ir1;
    "Test Arm_gen.ir 2" >:: test_ir2;
    "Test Arm_gen.ir 3" >:: test_ir3;
    "Test Arm_gen.ir 4" >:: test_ir4;
    "Test Arm_gen.ir 5" >:: test_ir5;
    "Test Arm_gen.ir 6" >:: test_ir6;
    "Test Arm_gen.ir 7" >:: test_ir7;
    "Test Arm_gen.ir 8" >:: test_ir8;
    "Test Arm_gen.ir 9" >:: test_ir9;
  ]
