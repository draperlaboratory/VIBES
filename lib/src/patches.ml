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

(* The names of the patches. *)
let patches = Map.of_alist_exn (module String) [
  "ret-3", Ret_3.bil;
  "ret-4", Ret_4.bil; ]

(* Helpers. *)
let names = Map.keys patches
let is_patch (name : string) : bool = List.mem names name ~equal:String.equal

(* [get_BIL "ret-3" 32] returns the [Ret_3.bil] BIL, with 32-bit words. *)
let get_BIL (name : string) (bits : int) : Bil.t KB.t =
  let patch = Map.find_exn patches name in
  let bil = patch bits in
  KB.return bil
