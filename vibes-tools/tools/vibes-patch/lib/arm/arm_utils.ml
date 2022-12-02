open Core
open Bap_core_theory

module CT = Vibes_utils.Core_theory
module Constants = Vibes_constants.Asm
module Ops = Vibes_select.Arm_ops
module Asm = Vibes_as.Types.Assembly
module Proc = Vibes_utils.Proc
module Filename = Stdlib.Filename

let (let*) x f = Result.bind x ~f

let assembler = "/usr/bin/arm-linux-gnueabi-as"
let objcopy = "/usr/bin/arm-linux-gnueabi-objcopy"

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
    ~directives:[".syntax unified"]
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

let has_ldr_large_const : Asm.t -> bool =
  let re = Str.regexp "ldr R[0-9]+, =[0-9]+" in
  fun asm -> Asm.blocks asm |> List.exists ~f:(fun block ->
      Asm.insns block |> List.exists ~f:(fun insn ->
          Str.string_match re insn 0))

let has_inline_data (asm : Asm.t) : bool =
  has_ldr_large_const asm

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

  let with_thumb
      (args : string list)
      (language : Theory.language) : string list =
    if CT.is_thumb language then "-mthumb" :: args else args

  let assemble
      (asm : Asm.t)
      (language : Theory.language) : (string, KB.conflict) result =
    let asmfile = Filename.temp_file "vibes" ".asm" in
    let objfile = Filename.temp_file "vibes" ".o" in
    let data = Format.asprintf "%a" Asm.pp asm in
    Out_channel.write_all asmfile ~data;
    let args = with_thumb ["-o"; objfile; asmfile] language in
    let* _ = Proc.run assembler args in
    Ok objfile

  let to_binary (objfile : string) : (string, KB.conflict) result =
    let binfile = Filename.temp_file "vibes" ".bin" in    
    let args = ["-O"; "binary"; objfile; binfile] in
    let* _ = Proc.run objcopy args in
    Ok (In_channel.read_all binfile)

end
