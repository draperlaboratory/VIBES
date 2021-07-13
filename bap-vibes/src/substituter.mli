(** Replaces higher variables with lower-level locations in s-exp patches. *)

open !Core_kernel

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

(** [substitute h_vars patch_code] replaces higher level variables [h_vars]
    with lower-level locations in the provided [patch_code]. *)
val substitute : Hvar.t list -> Sexp.t list -> Sexp.t list KB.t
