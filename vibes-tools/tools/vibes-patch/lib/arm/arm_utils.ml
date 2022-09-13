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

let trampoline (loc : int64) : Asm.block =
  let b = Format.sprintf "%s (%s + (%Ld))"
      (Ops.b ()) Constants.patch_start_label loc in
  let label = Format.sprintf "trampoline%Ld" loc in
  Asm.Fields_of_block.create ~label ~insns:[b]

let create_trampoline (loc : int64) : Asm.t =
  let block = trampoline loc in
  Asm.{directives = [".syntax unified"]; blocks = [block]}

let insert_trampoline (loc : int64) (asm : Asm.t) : Asm.t =
  let block = trampoline loc in
  Asm.{asm with blocks = asm.blocks @ [block]}

let has_ldr_large_const : Asm.t -> bool =
  let re = Str.regexp "ldr R[0-9]+, =[0-9]+" in
  fun asm -> Asm.blocks asm |> List.exists ~f:(fun block ->
      Asm.insns block |> List.exists ~f:(fun insn ->
          Str.string_match re insn 0))

let has_inline_data (asm : Asm.t) : bool =
  has_ldr_large_const asm

let adjusted_org (loc : int64) : int64 option =
  match Int64.rem loc 4L with
  | 0L -> None
  | org -> Some org

let situate
    ?(org : int64 option = None)
    ?(jmp : int64 option = None)
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
  match jmp with
  | Some jmp -> insert_trampoline Int64.(jmp - loc) asm
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
