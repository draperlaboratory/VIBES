
(** This is copied verbatim from the BAP flatten plugin with some modifications
    to deal with threading the knowledge base monad.
    We should submit a PR to put this functionality into bap.mli
    Flattening is required before sending of BIR to the minizinc instruction selector.
    Flattening converts complex right hand side expressions to multiple simple Def.t forms.
*)

open Bap.Std
open Bap_knowledge

val flatten_blk : Blk.t -> Blk.t Knowledge.t
