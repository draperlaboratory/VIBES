(** Passes relating to function calls and the ABI requirements thereof. *)

open Bap.Std
open Bap_core_theory
open Vibes_higher_vars

(** [insert_new_mems_at_callsites sub ~target] will insert assignments to
    a pseudo memory variable at each callsite in [sub]. Also returned is
    the set of tids for each of these assignments. This is used by the
    instruction selector to mark the most recent memory as a data dependency
    for each function call. *)
val insert_new_mems_at_callsites :
  sub term ->
  target:Theory.target ->
  sub term KB.t

(** [spill_hvars_and_adjust_stack sub ~target ~sp_align ~hvars] does the
    following:

    1. Compute the liveness information for [sub].

    2. Collect all variables that are live after a function call and are
       stored in a caller-saved register at the entry point (as specified
       by [hvars] and [target]).

    3. Create a stack frame to spill these registers to.

    4. Subtract [sp_align] from the stack pointer in order to keep it aligned
       as required by the ABI when making function calls.

    The result is the updated subroutine and the new higher vars information
    that refers to the variables according to their spilled locations on the
    stack.
*)
val spill_hvars_and_adjust_stack :
  sub term ->
  target:Theory.target ->
  sp_align:int ->
  hvars:Higher_var.t list ->
  (sub term * Higher_var.t list) KB.t
