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

(** 

   Applies a series of transformations to the patch code at the BIR level.
   These transformations include optimization opportunities, massaging the
   shape of the code for the instruction selector, and dealing with the
   limitations of the target architecture.

   Important: Tids of terms that are modified must be preserved. The only
   tids that shall change are the ones that are created fresh.

*)

open Core_kernel
open Bap.Std
open Bap_core_theory

(** The result of running our passes, which is a list of [blk term]s,
    as well as a set of registers to exclude from the Minizinc solution.
    Additionally, [argument_tids] holds the tids of each [def term] where
    an argument to a call was assigned. *)
type t = {
  ir : blk term list;
  cfg : Graphs.Tid.t;
  exclude_regs : String.Set.t;
  argument_tids : Tid.Set.t;
}

module Opt : sig

  (** [apply ir] applies [Term]-level optimizations to [ir], which are required
      to put the patch code in good shape before instruction selection. *)
  val apply : blk term list -> blk term list KB.t

end


module Shape : sig

  (** [reorder_blks ir] reorders [ir] according to a reverse post-order DFS
      traversal. *)
  val reorder_blks : blk term list -> blk term list KB.t

end

(** [run patch] creates the BIR from [patch], then applies a series of
    transformations to it. The resulting code is then ready to be handed
    off to the instruction selector. *)
val run : Data.Patch.t -> patch_spaces:Data.Patch_space_set.t -> t KB.t
