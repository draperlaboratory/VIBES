(** Replaces higher variables with lower-level locations in patches at
   the BIR level. *)

open !Core_kernel
open Bap_core_theory
open Bap.Std

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

(** [substitute h_vars patch_code] replaces higher level variables [h_vars]
    with lower-level locations in the provided [patch_code]. *)
val substitute : Theory.target -> Hvar.t list -> blk term list -> blk term list KB.t
