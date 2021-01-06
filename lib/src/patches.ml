(* This module contains some hand-written patches (for initial demos). *)

open !Core_kernel
open Bap.Std
open Bap_knowledge

module KB = Knowledge

(* Some BIL that returns 3. *)
module Ret_3 = struct

  let bil (bits : int) : Bil.t =
    let word_t = Type.imm bits in
    let r0 = Var.create "R0" word_t in
    let w = Word.of_int 3 ~width:bits in
    let three = Bil.int w in
    Bil.([
        r0 := three;
      ])

end

(* Some BIL that returns 4. *)
module Ret_4 = struct

  let bil (bits : int) : Bil.t =
    let word_t = Type.imm bits in
    let r0 = Var.create "R0" word_t in
    let w = Word.of_int 4 ~width:bits in
    let four = Bil.int w in
    Bil.([
        r0 := four;
      ])

end

module Test = struct

  let bil (bits : int) : Bil.t =
    let open Bil in
    let word_t = Type.imm bits in
    let v1 = Var.create "v1" word_t in
    let v2 = Var.create "v2" word_t in
    let v3 = Var.create "v3" word_t in
    let v4 = Var.create "v4" word_t in
    let int i = Word.of_int i ~width:bits |> Bil.int in
    [
      v1 := int 3;
      v2 := int 4;
      v3 := (var v1 + int 1) - var v2;
      v4 := (var v3 + int 1) lsl int 2;
    ]

end

(* The names of the patches. *)
let patches =
  Map.of_alist_exn (module String)
    [
      "test", Test.bil;
      "ret-3", Ret_3.bil;
      "ret-4", Ret_4.bil;
    ]

(* Helpers. *)
let names = Map.keys patches
let is_patch (name : string) : bool = List.mem names name ~equal:String.equal

(* [get_BIL "ret-3" 32] returns the [Ret_3.bil] BIL, with 32-bit words. *)
let get_BIL (name : string) (bits : int) : Bil.t KB.t =
  let patch = Map.find_exn patches name in
  let bil = patch bits in
  KB.return bil
