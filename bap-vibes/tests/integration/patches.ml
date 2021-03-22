(* This module contains some hand-written patches (for initial demos). *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

module KB = Knowledge

(* Some BIR that returns 3. *)
module Ret_3 = struct

  open Theory

  let prog (bits : int) : unit eff =
    Theory.instance
      ~context:["vibes"]
      ~requires:["bil"; "vibes:arm-gen"] () >>=
    Theory.require >>=
    fun (module Core) ->
    let open Core in
    let open Bap_vibes.Core_notations.Make(Core) in

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

(* The names of the patches. *)
let patches =
  Map.of_alist_exn (module String) [ "ret-3", Ret_3.prog; ]

(* Helpers. *)
let names = Map.keys patches

let is_patch (name : string) : bool = List.mem names name ~equal:String.equal

(* [get_bir "ret-3" 32] returns the [Ret_3.bil] BIL, with 32-bit words. *)
let get_bir (name : string) (bits : int) : Insn.t KB.t =
  let patch = Map.find_exn patches name in
  patch bits
