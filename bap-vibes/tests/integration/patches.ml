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
    Theory.instance () >>= Theory.require >>= fun (module Core) ->
    let open Core in
    let open Bap_vibes.Core_notations.Make(Core) in

    let word_t = Bitv.define bits in
    let r0 =
      Var.define word_t @@
      Bap_vibes.Substituter.Naming.mark_reg_name "R0" in
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
let patches : (int -> insn KB.t) String.Map.t =
  Map.of_alist_exn (module String) [ "ret-3", Ret_3.prog; ]

(* Helpers. *)
let names : string list = Map.keys patches
let is_patch (name : string) : bool = List.mem names name ~equal:String.equal

(* [get_sem "ret-3" 32] returns the [Ret_3.bil] BIL, with 32-bit words. *)
let get_sem (name : string) (bits : int) : insn KB.t =
  let patch = Map.find_exn patches name in
  patch bits
