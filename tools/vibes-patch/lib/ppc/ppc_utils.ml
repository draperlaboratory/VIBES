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

module CT = Vibes_utils.Core_theory
module Constants = Vibes_constants.Asm
module Ops = Vibes_select.Arm_ops
module Asm = Vibes_as.Types.Assembly
module Proc = Vibes_utils.Proc
module Filename = Stdlib.Filename
module Log = Vibes_log.Stream

let (let*) x f = Result.bind x ~f

let assembler : string = "/usr/bin/powerpc-linux-gnu-as"
let objcopy : string = "/usr/bin/powerpc-linux-gnu-objcopy"
let objdump : string = "/usr/bin/powerpc-linux-gnu-objdump"

let max_insn_length : int = 4

let trampoline (addr : int64) : Asm.block =
  let op = Ops.b () in
  let b = Format.sprintf "%s (%s + %Ld - %s)"
      op
      Constants.patch_start_label
      addr
      Constants.patch_location in
  let label = Format.sprintf "trampoline%Ld" addr in
  Asm.Fields_of_block.create ~label ~insns:[b]

let create_trampoline
    (addr : int64)
    (patch_point : int64)
    (patch_size : int64) : Asm.t =
  let block = trampoline addr in
  Asm.Fields.create ~patch_point ~patch_size
    ~directives:[]
    ~blocks:[block]

let insert_trampoline (addr : int64) (asm : Asm.t) : Asm.t =
  let block = trampoline addr in
  Asm.{asm with blocks = asm.blocks @ [block]}

let insert_overwritten
    (addr : int64)
    (asm : Asm.t)
    (insns : string list) : Asm.t =
  let label = Format.sprintf "overwritten%Ld" addr in
  let block = Asm.Fields_of_block.create ~label ~insns in
  Asm.{asm with blocks = asm.blocks @ [block]}

let has_inline_data (_asm : Asm.t) : bool = false

let ends_in_jump (asm : Asm.t) : bool = match List.last asm.blocks with
  | None -> false
  | Some block -> match List.last block.insns with
    | None -> false
    | Some insn -> String.is_prefix insn ~prefix:"b "

let adjusted_org (loc : int64) : int64 option =
  match Int64.rem loc 4L with
  | 0L -> None
  | org -> Some org

let situate
    ?(org : int64 option = None)
    ?(jmp : int64 option = None)
    ?(overwritten : string list = [])
    (asm : Asm.t)
    ~(loc : int64)
    ~(to_addr : int64 -> int64) : Asm.t =
  let asm =
    let dir =
      Format.sprintf ".equiv %s, %Ld"
        Constants.patch_location @@ to_addr loc in
    Asm.{asm with directives = asm.directives @ [dir]} in
  let asm = match org with
    | None -> asm
    | Some org ->
      let dir = Format.sprintf ".org %Ld" org in
      Asm.{asm with directives = asm.directives @ [dir]} in
  let asm =
    let label = Constants.patch_start_label in
    let start = Asm.Fields_of_block.create ~label ~insns:[] in
    Asm.{asm with blocks = start :: asm.blocks} in
  let asm =
    if Int64.(asm.Asm.patch_size = 0L)
    && not (List.is_empty overwritten) then
      insert_overwritten (to_addr loc) asm overwritten
    else asm in
  match jmp with
  | Some jmp -> insert_trampoline jmp asm
  | None -> asm


module Toolchain = struct

  let long_re : Str.regexp =
    Str.regexp {| +[1-9a-f]+: +[0-9a-f]+ +\.long +0x[0-9a-f]+|}

  (* This is a bit hacky, but we want to avoid the complexity of disassembling
     the file ourselves. *)
  let calculate_inline_data (objfile : string) : (int, KB.conflict) result =
    Log.send "Calculating inline data for %s" objfile;
    let* out, _ = Proc.run objdump ["-S"; objfile] in
    let n = List.fold out ~init:0 ~f:(fun n s ->
        let s = String.map s ~f:(function
            | '\t' -> ' ' | c -> c) in
        if Str.string_match long_re s 0 then n + 4
        else n) in
    Log.send "Found %d bytes of data" n;
    Ok n

  let assemble
      (asm : Asm.t)
      (_language : Theory.language) : (string * int, KB.conflict) result =
    let asmfile = Filename.temp_file "vibes" ".asm" in
    let objfile = Filename.temp_file "vibes" ".o" in
    let data = Format.asprintf "%a" Asm.pp asm in
    Out_channel.write_all asmfile ~data;
    let* _ = Proc.run assembler ["-o"; objfile; asmfile] in
    let* data_size = calculate_inline_data objfile in
    Ok (objfile, data_size)

  let to_binary (objfile : string) : (string, KB.conflict) result =
    let binfile = Filename.temp_file "vibes" ".bin" in
    let args = ["-O"; "binary"; objfile; binfile] in
    let* _ = Proc.run objcopy args in
    Ok (In_channel.read_all binfile)

end
