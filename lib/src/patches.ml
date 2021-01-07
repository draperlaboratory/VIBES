(* This module contains some hand-written patches (for initial demos). *)

open !Core_kernel
open Bap_knowledge
open Bap_core_theory

module KB = Knowledge
open KB.Let

(* Some BIR that returns 3. *)
module FRet_3 (S : Theory.Core) = struct

  open Theory
  open S
  open Core_notations.Make(S)

  let prog (bits : int) : unit eff =
    let word_t = Bitv.define bits in
    let r0 = Var.define word_t "R0" in
    let three = int word_t Bitvec.M32.(!!3) in
    let data = data_body
        [
          r0 := three;
        ]
    in
    let ctrl = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "patch" in
    blk l data ctrl

end

(* Some BIL that returns 4. *)
module FRet_4 (S : Theory.Core) = struct

  open Theory
  open S
  open Core_notations.Make(S)

  let prog (bits : int) : unit eff =
    let word_t = Bitv.define bits in
    let r0 = Var.define word_t "R0" in
    let four = int word_t Bitvec.M32.(!!4) in
    let data = data_body
        [
          r0 := four;
        ]
    in
    let ctrl = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "patch" in
    blk l data ctrl

end

module FTest (S : Theory.Core) = struct

  open Theory
  open S
  open Core_notations.Make(S)

  let prog (bits : int) : unit eff =
    let word_t = Bitv.define bits in
    let v1 = Var.define word_t "v1" in
    let v2 = Var.define word_t "v2" in
    let v3 = Var.define word_t "v3" in
    let v4 = Var.define word_t "v4" in
    let int i = int word_t Bitvec.M32.(!!i) in
    let data = data_body
        [
          v1 := int 3;
          v2 := int 4;
          v3 := (var v1 + int 1) - var v2;
          v4 := (var v3 + int 1) << int 2;
        ]
    in
    let ctrl = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "patch" in
    blk l data ctrl

end

module ARM = Arm_gen.ARM_Core

module Ret_3 = FRet_3(ARM)

module Ret_4 = FRet_4(ARM)

module Test = FTest(ARM)

(* The names of the patches. *)
let patches =
  Map.of_alist_exn (module String)
    [
      "ret-3", Ret_3.prog;
      "ret-4", Ret_4.prog;
      "test", Test.prog;
    ]

(* Helpers. *)
let names = Map.keys patches

let is_patch (name : string) : bool = List.mem names name ~equal:String.equal

(* [get_bir "ret-3" 32] returns the [Ret_3.bil] BIL, with 32-bit words. *)
let get_bir (name : string) (bits : int) : Theory.Program.t KB.t =
  let patch = Map.find_exn patches name in
  let+ bir = patch bits in
  KB.Value.put Theory.Semantics.slot Theory.Program.empty bir
