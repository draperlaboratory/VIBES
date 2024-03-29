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

open Core
open Bap_core_theory

module Assembly = struct

  type block = {
    label : string;
    insns : string list;
  } [@@deriving fields, sexp]

  type t = {
    patch_point : int64;
    patch_size : int64;
    directives : string list;
    blocks : block list;
  } [@@deriving fields, sexp]

  type printer =
    Vibes_ir.Types.t ->
    Vibes_patch_info.Types.t ->
    (t, KB.conflict) result

  let pp_block (ppf : Format.formatter) (blk : block) : unit =
    Format.fprintf ppf "%s:\n%!" blk.label;
    List.iter blk.insns ~f:(Format.fprintf ppf "    %s\n%!")

  let pp (ppf : Format.formatter) (asm : t) : unit =
    List.iter asm.directives ~f:(Format.fprintf ppf "%s\n\n%!");
    List.iter asm.blocks ~f:(Format.fprintf ppf "%a%!" pp_block)

end
