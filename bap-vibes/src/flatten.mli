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


(** This is copied verbatim from the BAP flatten plugin with some modifications
    to deal with threading the knowledge base monad.
    We should submit a PR to put this functionality into bap.mli
    Flattening is required before sending of BIR to the minizinc instruction selector.
    Flattening converts complex right hand side expressions to multiple simple Def.t forms.
*)

open Bap.Std
open Bap_knowledge

val flatten_blk : Blk.t -> Blk.t Knowledge.t
