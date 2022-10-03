(** Passes for normalizing the shape/layout of the program before
    instruction selection. *)

open Bap.Std
open Bap_core_theory

(** Removes unreachable blocks from the program. Returns [Error] if
    the program is empty. *)
val remove_unreachable : sub term -> (sub term, KB.conflict) result

(** Normalizes the shape of exit blocks in the program. Exit blocks
    can either continue execution normally by "falling through" to
    the end of the patch point, or they can abort the normal control
    to somewhere else in the binary.

    The normalization that is done is that there is only one such
    block that implicitly "falls through" (redirecting existing
    blocks to this one if necessary).
*)
val adjust_exits : sub term -> sub term KB.t

(** Reorders the blocks in the program according to a reverse DFS
    postorder. *)
val reorder_blks : sub term -> sub term

(** For architectures with a limited range for conditional branches
    (as specified by [fwd_limit] and [bwd_limit]), we will make a
    conservative estimate based on the available patch spaces in
    [patch_info] as to whether such branches need to be relaxed
    in order to satisfy the range requirement.

    If they need to be relaxed, the translation will create a new
    block for this conditional branch to target, where the real branch
    destination is targeted by an unconditional branch.
*)
val relax_branches :
  ?patch_spaces:Vibes_patch_info.Types.Spaces.t ->
  sub term ->
  target:Theory.target ->
  patch_info:Vibes_patch_info.Types.t ->
  fwd_limit:int ->
  bwd_limit:int ->
  sub term KB.t

(** Moves conditional branches to their own blocks.

    This remedies a shortcoming of the instruction selector, which
    is that we don't model updates to flag registers, so the scheduler
    may produce incorrect solutions. Currently this only applies to
    Thumb.
*)
val split_on_conditional : sub term -> sub term KB.t

(** Turns conditional calls into a conditional jump to a new block,
    where the call is then performed. *)
val split_conditional_calls : sub term -> sub term KB.t
