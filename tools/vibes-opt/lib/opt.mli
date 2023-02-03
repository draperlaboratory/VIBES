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

(** General BIR optimization passes.

    Invariants:

    - The subroutine is not in SSA form.
    - The subroutine's block ordering is in reverse DFS
      postorder (see [Shape.reorder_blks]).
*)

open Bap.Std
open Bap_core_theory
open Vibes_higher_vars

type t = Higher_var.t list -> sub term -> sub term KB.t

module type S = sig val go : t end

(** Copy of BAP's optimization passes:

    https://github.com/BinaryAnalysisPlatform/bap/tree/master/plugins/optimization

    Performs constant folding, dead code elimination, and
    constant propagation.
*)
module Builtin : S

(** Block merging. *)
module Merge : S

(** Edge contraction. *)
module Contract : S

(** Registers an optimization pass. By default the above passes
    are included. *)
val register : (module S) -> unit

(** Applies the optimization passes. *)
val apply : t
