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
open Bap.Std
open Bap_elf.Std
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Constants = Vibes_constants.Asm
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces
module Asm = Vibes_as.Types.Assembly
module Log = Vibes_log.Stream
module Dis = Disasm_expert.Basic

type dis = (Dis.asm, Dis.kinds) Dis.t

let (let*) x f = Result.bind x ~f
let (@.) = Fn.compose

type target = (module Types.Target)

let target_info
    (target : T.target)
    (language : T.language) : (dis * target, KB.conflict) result =
  let* info =
    if CT.is_arm32 target then
      let module Target : Types.Target = struct
        include Arm_utils
        let target = target
      end in
      Ok (module Target : Types.Target)
    else if CT.is_ppc32 target then
      let module Target : Types.Target = struct
        include Ppc_utils
        let target = target
      end in
      Ok (module Target : Types.Target)
    else
      let msg = Format.asprintf "Unsupported target %a" T.Target.pp target in
      Error (Errors.Unsupported_target msg) in
  match Dis.lookup target language with
  | Ok dis -> Ok (Dis.(store_asm @@ store_kinds dis), info)
  | Error err ->
    Error (Errors.No_disasm (Format.asprintf "%a" Error.pp err))

(* Find the code segment in the binary, and see how much room we have
   for extending it when we need more space. *)
module Code_seg = struct

  type t = {
    addr : int64;
    size : int64;
    end_ : int64;
    off  : int64;
    room : int64;
  }

  let find (spec : Ogre.doc) : t option =
    let open Int64 in
    let (let*) x f = Option.bind x ~f in
    let query = Ogre.Query.(select @@ from Image.Scheme.segment) in
    let* segs = Ogre.eval (Ogre.collect query) spec |> Or_error.ok in
    let* code_addr, code_size = Seq.find_map segs ~f:(fun seg ->
        let {Image.Scheme.addr; size; info=(_,_,x)} = seg in
        Option.some_if x (addr, size)) in
    let* region = Utils.find_mapped_region code_addr spec in
    let code_end = code_addr + code_size in
    let closer x y = x - code_end < y - code_end in
    let next_addr = Seq.fold segs ~init:None ~f:(fun acc seg ->
        let {Image.Scheme.addr; _} = seg in
        if addr >= code_end then match acc with
          | Some a when closer addr a -> Some addr
          | Some _ -> acc
          | None -> Some addr
        else acc) in
    let size = match next_addr with
      | Some a -> a - code_end
      | None -> 0L in
    Log.send "Code segment end = 0x%Lx, available space = %Ld bytes"
      code_end size;
    Some {
      addr = code_addr;
      size = code_size;
      end_ = code_end;
      off  = region.offset;
      room = size;
    }

end

type info = {
  spec     : Ogre.doc;
  target   : target;
  language : T.language;
  dis      : dis;
  memmap   : value memmap;
  code     : Code_seg.t;
}

let create_info
    (spec : Ogre.doc)
    (memmap : value memmap)
    (target : T.target)
    (language : T.language) : (info, KB.conflict) result =
  let* dis, target = target_info target language in
  match Code_seg.find spec with
  | None -> Error (Errors.No_code_segment "No code segment found")
  | Some code -> Ok {spec; target; language; dis; memmap; code}

type patch = {
  data       : string;
  addr       : int64;
  loc        : int64;
  inline     : int;
  root       : int64;
  trampoline : bool;
}

let pp_patch (ppf : Format.formatter) (p : patch) : unit =
  let len = String.length p.data in
  Format.fprintf ppf "addr=0x%Lx, \
                      offset=0x%Lx, \
                      len=%d, \
                      inline=%d, \
                      root=0x%Lx, \
                      trampoline=%b"
    p.addr p.loc len p.inline p.root p.trampoline

(* If the patch placement overwrites instructions that need to be
   preserved in the binary, then we will disassemble those instructions
   and use them as part of the patch assembly.

   TODO: what if the overwritten instructions have PC-relative operands?
   If we try to place them at a different location in the binary then it
   would probably break the behavior. The easy solution is to fail and
   then tell the user that they picked a bad patch point.
*)
module Overwritten = struct

  let find_mem
      (target : T.target)
      (addr : int64)
      (size : int64)
      (memmap : value memmap) : (mem, KB.conflict) result =
    let open Int64 in
    let len_ok mem = size <= of_int (Memory.length mem) in
    let width = T.Target.code_addr_size target in
    let from = Word.of_int64 ~width addr in
    Memmap.to_sequence memmap |> Seq.find_map ~f:(fun (mem, _) ->
        if Memory.contains mem from && len_ok mem then
          Memory.view mem ~from |> Result.ok
        else None) |> function
    | Some mem -> Ok mem
    | None ->
      let msg = Format.asprintf
          "Couldn't find memory of at least size %Ld for patch point %a"
          size Word.pp from in
      Error (Errors.Invalid_address msg)

  let go
      (dis : dis)
      (memmap : value memmap)
      (target : T.target)
      (addr : int64)
      (size : int64) : (string list * int64, KB.conflict) result =
    let open Int64 in
    let invalid_size t =
      let msg = Format.asprintf
          "Invalid size %Ld at 0x%Lx (%Ld bytes disassembled)"
          addr size t in
      Error (Errors.Invalid_size msg) in
    let rec disasm acc mem t n = match Dis.insn_of_mem dis mem with
      | Error err ->
        let msg = Format.asprintf "Disasm error: %a" Error.pp err in
        Error (Errors.Invalid_insn msg)
      | Ok (_, None, _) ->
        (* The bytes at this address failed to decode to a valid instruction,
           but we should just assume that it's free space (i.e. inline data)
           and that the user intends to overwrite this region. *)
        Ok (List.rev acc, t)
      | Ok (m, Some insn, next) ->
        let len = of_int @@ Memory.length m in
        let n = n - len and t = t + len in
        let asm = Dis.Insn.asm insn in
        if n = 0L then Ok (List.rev (asm :: acc), t)
        else match next with
          | `finished -> invalid_size t
          | `left _ when n < 0L -> Ok (List.rev (asm :: acc), t)
          | `left mem -> disasm (asm :: acc) mem t n in
    let* mem = find_mem target addr size memmap in
    let* insns, n = disasm [] mem 0L size in
    if not @@ List.is_empty insns then
      Log.send "The following instructions will be overwritten at 0x%Lx:\n%s\n"
        addr @@ String.concat insns ~sep:"\n";
    Ok (insns, n)

end

module Elf = struct

  include Elf

  type iseg = int * Elf.segment

  let extend_seg
      (elf : Elf.t)
      ((i, seg) : iseg)
      (data : bigstring)
      (size : int64) : unit =
    let open Int64 in
    let rd16le pos = Bigstring.get_int16_le data ~pos |> of_int in
    let rd16be pos = Bigstring.get_int16_be data ~pos |> of_int in
    let rd32le pos = Bigstring.get_int32_t_le data ~pos |> of_int32 in
    let rd32be pos = Bigstring.get_int32_t_be data ~pos |> of_int32 in
    let rd64le pos = Bigstring.get_int64_t_le data ~pos in
    let rd64be pos = Bigstring.get_int64_t_be data ~pos in
    let wr32le pos = Bigstring.set_int32_t_le data ~pos @. to_int32_trunc in
    let wr32be pos = Bigstring.set_int32_t_be data ~pos @. to_int32_trunc in
    let wr64le pos = Bigstring.set_int64_t_le data ~pos in
    let wr64be pos = Bigstring.set_int64_t_be data ~pos in
    let rd16, rd, wr, fs, ms, ps, po = match elf.e_class, elf.e_data with
      | ELFCLASS32, ELFDATA2LSB -> rd16le, rd32le, wr32le, 16L, 20L, 42, 28
      | ELFCLASS32, ELFDATA2MSB -> rd16be, rd32be, wr32be, 16L, 20L, 42, 28
      | ELFCLASS64, ELFDATA2LSB -> rd16le, rd64le, wr64le, 24L, 32L, 54, 32
      | ELFCLASS64, ELFDATA2MSB -> rd16be, rd64be, wr64be, 24L, 32L, 54, 32 in
    Log.send "Extending segment 0x%Lx at index %d by %Ld bytes"
      seg.p_vaddr i size;
    let phentsize, phoff = rd16 ps, rd po in
    let entry = phoff + of_int i * phentsize in
    wr (to_int_exn (entry + fs)) (seg.p_filesz + size);
    wr (to_int_exn (entry + ms)) (seg.p_memsz  + size)

  let is_pt_load (seg : segment) : bool = match seg.p_type with
    | PT_LOAD -> true
    | _ -> false

  let is_pf_exec (seg : segment) : bool =
    List.exists seg.p_flags ~f:(function
        | PF_X -> true
        | _ -> false)

  let find_seg (elf : t) (addr : int64) : (iseg, KB.conflict) result =
    let open Int64 in
    Seq.find_mapi elf.e_segments ~f:(fun i seg ->
        if seg.p_vaddr = addr && is_pt_load seg && is_pf_exec seg
        then Some (i, seg) else None) |> function
    | Some x -> Ok x
    | None ->
      let msg = Format.sprintf
          "Segment at 0x%Lx was not found in the binary"
          addr in
      Error (Errors.Invalid_binary msg)

end

module Placement = struct

  (* The available space after the code segment won't be marked by BAP
     as a valid code region, so we will pretend it exists.

     We look for code regions in the OGRE doc under the assumption that
     not all mapped regions will be code (or even executable), therefore
     if the user tried to place a patch in a non-code region we would
     end up with a broken binary.
  *)
  let extern_region (info : info) (addr : int64) : Utils.region option =
    let open Int64 in
    if addr >= info.code.end_
    && addr < info.code.end_ + info.code.room then Some Utils.{
        addr = info.code.end_;
        size = info.code.room;
        offset = info.code.off + info.code.size;
      } else Utils.find_code_region addr info.spec

  (* Get the raw binary data of the patch at the specified patch site. *)
  let situate
      (info : info)
      (asm : Asm.t)
      (org : int64 option)
      (to_addr : int64 -> int64)
      (loc : int64)
      (overwritten : string list)
      (jmp : int64 option) : (string * int, KB.conflict) result =
    let module Target = (val info.target) in
    let asm' = Target.situate asm ~loc ~to_addr ~org ~jmp ~overwritten in
    Log.send "Situated assembly at 0x%Lx:\n%a" (to_addr loc) Asm.pp asm';
    let* objfile, inline_data = Target.Toolchain.assemble asm' info.language in
    let* data = Target.Toolchain.to_binary objfile in
    (* If the origin was adjusted then shave off those bytes. *)
    let data = match org with
      | Some org -> String.drop_prefix data @@ Int64.to_int_exn org
      | None -> data in
    Ok (data, inline_data)

  (* Try to fit the patch into the specified patch site. *)
  let try_site
      ?(trampoline : bool = false)
      ?(extern : bool = false)
      ?(ret : int64 option)
      ?(overwritten : string list = [])
      (info : info)
      (region : Utils.region)
      (addr : int64)
      (size : int64)
      (asm : Asm.t) : (patch option, KB.conflict) result =
    let open Int64 in
    let module Target = (val info.target) in
    let root = asm.patch_point in
    let loc = Utils.addr_to_offset addr region in
    let to_addr loc = Utils.offset_to_addr loc region in
    let org = Target.adjusted_org addr in
    let situate = situate info asm org to_addr loc overwritten in
    let end_jmp = Target.ends_in_jump asm in
    let inline_data = Target.has_inline_data asm in
    let needs_jmp = not end_jmp && (extern || inline_data) in
    let* data, inline = situate @@ match ret with
      | Some _ when needs_jmp -> ret
      | None when needs_jmp -> assert false
      | None | Some _ -> None in
    match of_int @@ String.length data with
    | len when len = size ->
      (* The patch uses exactly the available space. *)
      Ok (Some {data; addr; loc; inline; root; trampoline})
    | len when len < size && (end_jmp || needs_jmp) ->
      (* We have some space left over, but the patch already has
         a jump at the end. *)
      Ok (Some {data; addr; loc; inline; root; trampoline})
    | len when len < size ->
      (* For patching at the original location, we need to insert
         a jump to the end of the specified space. We need to check
         that the patch still fits because inserting a jump will
         increase the length of the patch. *)
      Log.send "Patch has %Ld bytes of space remaining, \
                requires a jump" (size - len);
      let* data, inline = situate @@ match ret with
        | None -> assert false
        | Some _ -> ret in
      Result.return begin match of_int @@ String.length data with
        | len when len <= size ->
          Some {data; addr; loc; inline; root; trampoline}
        | len ->
          Log.send "Patch doesn't fit at address 0x%Lx (%Ld bytes \
                    available, %Ld bytes needed)" addr size len;
          None
      end
    | len ->
      Log.send "Patch doesn't fit at address 0x%Lx (%Ld bytes \
                available, %Ld bytes needed)" addr size len;
      Ok None

  let collect_overwritten
      (info : info)
      (patch : patch)
      (patch_name : string)
      (addr : int64)
      (size : int64) : (string list * int64, KB.conflict) result =
    let open Int64 in
    let module Target = (val info.target) in
    (* Bytes required for the patch. *)
    let len = of_int @@ String.length patch.data in
    Log.send "%s fits (%Ld bytes)" patch_name len;
    (* If the patch is bigger than the number of bytes
       we intended to replace, then we need to disassemble
       the remaining instructions that would have been
       overwritten. *)
    let osize = len - size in
    if osize > 0L then
      (* Start at the end of the instructions that we intended
         to overwrite. *)
      let oaddr = addr + size in
      Log.send "%Ld bytes remain, need to disassemble at 0x%Lx"
        osize oaddr;
      Overwritten.go info.dis info.memmap Target.target oaddr osize
    else Ok ([], size)

  (* Using an external patch space requires a trampoline. *)
  type extern = {
    patch      : patch;
    trampoline : patch;
  }

  type spaces = Patch_info.space list

  (* Try the provided external patch spaces. *)
  let rec extern
      (info : info)
      (orig_region : Utils.region)
      (asm : Asm.t)
      (addr : int64)
      (size : int64) : spaces -> (extern, KB.conflict) result = function
    | [] -> Error (Errors.No_patch_spaces "No suitable patch spaces found")
    | space :: rest ->
      let open Int64 in
      let module Target = (val info.target) in
      let jumpto = Bitvec.to_int64 @@ Word.to_bitvec space.address in
      let next () = extern info orig_region asm addr size rest in
      Log.send "Attempting patch space 0x%Lx with %Ld bytes of space"
        jumpto space.size;
      match extern_region info jumpto with
      | None ->
        Log.send "Code region for patch space at address \
                  0x%Lx was not found" jumpto;
        next ()
      | Some region ->
        (* We have to make sure that the jump to the external space
           will fit at the intended patch point. *)
        Log.send "Found region (addr=0x%Lx, size=%Ld, offset=0x%Lx), \
                  attempting to situate trampoline"
          region.addr region.size region.offset;
        let* trampoline =
          let maxlen = of_int Target.max_insn_length in
          let asm = Target.create_trampoline jumpto addr size in
          try_site info orig_region addr maxlen asm ~trampoline:true in
        let trampoline_name = Format.sprintf "Trampoline 0x%Lx" addr in
        match trampoline with
        | None ->
          Log.send "%s doesn't fit" trampoline_name;
          next ()
        | Some trampoline ->
          let* overwritten, n =
            collect_overwritten info trampoline trampoline_name addr size in
          let* patch = try_site info region jumpto space.size asm
              ~overwritten ~extern:true ~ret:(addr + n) in
          match patch with
          | Some patch -> Ok {patch; trampoline}
          | None -> next ()

  type placed =
    | Local of patch
    | Extern of extern

  (* Search for a space in the binary where the patch will fit. We first
     try the provided patch point, and if it doesn't work we will try the
     available external patch spaces that the user gave us. *)
  let place_one
      (info : info)
      (patch_spaces : spaces)
      (asm : Asm.t) : (placed, KB.conflict) result =
    let open Int64 in
    let module Target = (val info.target) in
    let addr, size = asm.patch_point, asm.patch_size in
    Log.send "Attempting patch point 0x%Lx with %Ld bytes of space" addr size;
    let* region = match Utils.find_code_region addr info.spec with
      | Some region -> Ok region
      | None ->
        let msg = Format.sprintf
            "Couldn't find code region for patch point 0x%Lx"
            addr in
        Error (Errors.Invalid_address msg) in
    Log.send "Found region (addr=0x%Lx, size=%Ld, offset=0x%Lx)"
      region.addr region.size region.offset;
    let* patch =
      if size > 0L then
        try_site info region addr size asm ~ret:(addr + size)
      else begin
        Log.send "Patch size is zero, need external patch space";
        Ok None
      end in
    match patch with
    | Some patch -> Ok (Local patch)
    | None ->
      let* e = extern info region asm addr size patch_spaces in
      Ok (Extern e)

  (* Find the space that was used for the patch, and consume that part of it.
     If there is no more space left, then remove it from the list. *)
  let consume (patch : patch) (spaces : spaces) : spaces =
    List.filter_map spaces ~f:(fun space ->
        let open Int64 in
        let Patch_info.{address; size} = space in
        let width = Word.bitwidth address in
        let address = Bitvec.to_int64 @@ Word.to_bitvec address in
        if patch.addr = address then
          let patch_size = of_int @@ String.length patch.data in
          let size = size - patch_size in
          if size > 0L then
            let address = Word.of_int64 ~width (address + patch_size) in
            Some Patch_info.{address; size}
          else None
        else Some space)

  type asms = Asm.t list

  (* - `patches` is the list of patches that were successfully placed.

     - `spaces` is the list of external patch sites after placement,
       where the space has been consumed.

     - `remaining` is the list of patches (in assembly) that have not
       been placed yet.
  *)
  type t = {
    patches   : patch list;
    spaces    : spaces;
    remaining : asms;
  }

  let rec go
      ?(acc : patch list = [])
      (info : info)
      (spaces : spaces) : asms -> (t, KB.conflict) result = function
    | [] -> Ok {patches = List.rev acc; spaces; remaining = []}
    | (asm :: rest) as asms -> match place_one info spaces asm with
      | Error (Errors.No_patch_spaces _) ->
        (* We ran out of room, but we can try to create space in the
           binary later. *)
        Ok {patches = List.rev acc; spaces; remaining = asms}
      | Error _ as err -> err
      | Ok (Local patch) ->
        Log.send "Solved patch placement: %a" pp_patch patch;
        go info spaces rest ~acc:(patch :: acc)
      | Ok (Extern {patch; trampoline = t}) ->
        Log.send "Solved patch placement: %a" pp_patch patch;
        Log.send "Trampoline: %a" pp_patch t;
        go info (consume patch spaces) rest ~acc:(t :: patch :: acc)

end

module Extend_ogre = struct

  type res = (Ogre.doc, KB.conflict) result

  type len = {
    total : int64;
    data  : int64;
    code  : int64;
  }

  let not_extern (patch : patch) (region : Utils.named_region) : bool =
    Int64.(patch.addr >= region.addr &&
           patch.addr < region.addr + region.size)

  let length (patch : patch) : len =
    let open Int64 in
    let total = of_int @@ String.length patch.data in
    let data = of_int patch.inline in
    let code = total - data in
    {total; data; code}

  let provide_code
      (patch : patch)
      (name : string)
      (root : int64)
      (len : len) : unit Ogre.t =
    let module S = Image.Scheme in
    Ogre.sequence [
      Ogre.provide S.mapped patch.addr len.code patch.loc;
      Ogre.provide S.code_region patch.addr len.code patch.loc;
      Ogre.provide S.named_region patch.addr len.code name;
      Ogre.provide S.symbol_chunk patch.addr len.code root;
    ]

  let provide_data (patch : patch) (len : len) : unit Ogre.t =
    let open Int64 in
    if len.data > 0L then
      let module S = Image.Scheme in
      let addr = patch.addr + len.code in
      let loc = patch.loc + len.code in
      let name = Format.sprintf "data_%Lx" addr in
      Ogre.sequence [
        Ogre.provide S.mapped addr len.data loc;
        Ogre.provide S.segment addr len.data true false false;
        Ogre.provide S.named_region addr len.data name;
      ]
    else Ogre.return ()

  let provide
      (patch : patch)
      (region : Utils.named_region)
      (name : string)
      (root : int64) : unit Ogre.t =
    let len = length patch in
    let code =
      (* If we're not using an external patch space then we
         won't need to include the code information about it,
         since it already exists in the spec. However, the
         patch may have still included some inline data, so
         we should be sure to provide that information. *)
      if not_extern patch region then Ogre.return ()
      else provide_code patch name root len in
    Ogre.sequence [code; provide_data patch len]

  let find_root_name
      (ogre : Ogre.doc)
      (region : Utils.named_region) : (string, KB.conflict) result =
    match Utils.find_symbol_chunk region.addr ogre with
    | None ->
      let msg = Format.asprintf
          "No symbol chunk for named region 0x%Lx"
          region.addr in
      Error (Errors.Invalid_ogre msg)
    | Some chunk -> match Utils.find_named_symbol chunk.root ogre with
      | Some name -> Ok name
      | None ->
        let msg = Format.asprintf
            "No named symbol for symbol chunk root 0x%Lx"
            chunk.root in
        Error (Errors.Invalid_ogre msg)

  let named_region
      (ogre : Ogre.doc)
      (patch : patch) : (Utils.named_region, KB.conflict) result =
    match Utils.find_named_region patch.root ogre with
    | Some region -> Ok region
    | None ->
      let msg = Format.asprintf
          "No named region for patch 0x%Lx, root 0x%Lx"
          patch.addr patch.root in
      Error (Errors.Invalid_ogre msg)

  let one (ogre : Ogre.doc) (patch : patch) : res =
    let* region = named_region ogre patch in
    let* name = find_root_name ogre region in
    let name = Format.sprintf "%s@%Lx" name patch.addr in
    match Ogre.exec (provide patch region name region.addr) ogre with
    | Ok _ as res -> res
    | Error err ->
      let msg = Format.asprintf
          "Failed to extend OGRE file for patch 0x%Lx, root 0x%Lx: %a"
          patch.addr patch.root Error.pp err in
      Error (Errors.Invalid_ogre msg)

  let rec go (ogre : Ogre.doc) : patch list -> res = function
    | [] -> Ok ogre
    | p :: rest when p.trampoline -> go ogre rest
    | p :: rest ->
      let* ogre = one ogre p in
      go ogre rest

end

type t = {
  patches  : patch list;
  spaces   : Spaces.t;
  new_ogre : Ogre.doc option;
}

(* Copy the original binary and write the patches to it. *)
let write
    ?(extend : int64 = 0L)
    (code_seg_addr : int64)
    (patches : patch list)
    (binary : string)
    (patched_binary : string) : (unit, KB.conflict) result =
  let open Int64 in
  let data = Bigstring.of_string @@ In_channel.read_all binary in
  Log.send "Reading ELF headers";
  let* elf, seg = match Elf.from_bigstring data with
    | Error _ -> Error (Errors.Invalid_binary "Invalid ELF header")
    | Ok elf ->
      let* seg = Elf.find_seg elf code_seg_addr in
      Ok (elf, seg) in
  Result.return @@ Out_channel.with_file patched_binary ~f:(fun file ->
      if extend > 0L then Elf.extend_seg elf seg data extend;
      Out_channel.output_string file @@ Bigstring.to_string data;
      List.iter patches ~f:(fun patch ->
          Out_channel.seek file patch.loc;
          Out_channel.output_string file patch.data))

let patch
    ?(ogre : Ogre.doc option = None)
    ?(patch_spaces : Spaces.t = Spaces.empty)
    (target : T.target)
    (language : T.language)
    (asms : Asm.t list)
    ~(binary : string)
    ~(patched_binary : string) : (t, KB.conflict) result =
  Log.send "Loading binary %s" binary;
  let* image = Vibes_utils.Loader.image binary in
  let memmap = Image.memory image in
  let spec = Image.spec image in
  let* info = create_info spec memmap target language in
  Log.send "Solving patch placement";
  let spaces = Spaces.to_list patch_spaces in
  let had_spaces = not @@ List.is_empty spaces in
  let* placement = Placement.go info spaces asms in
  let* patches, extend = match placement.remaining with
    | [] -> Ok (placement.patches, 0L)
    | remaining ->
      (* Either the user didn't give us any external patch spaces
         or we used up all the available space. Our last resort is
         to increase the size of the code segment so that we can
         place the remaining patches there. *)
      if had_spaces
      then Log.send "Ran out of patch spaces"
      else Log.send "No patch spaces provided";
      Log.send "Attempting to create space";
      let width = T.Target.code_addr_size target in
      let space = Patch_info.{
          address = Word.of_int64 ~width info.code.end_;
          size = info.code.room;
        } in
      let* rest = Placement.go info [space] remaining in
      if List.is_empty rest.remaining then
        let open Int64 in
        (* Find out how many bytes by which we need to extend the
           code segment. *)
        let extend = match rest.spaces with
          | [] -> info.code.room
          | s :: _ -> Word.to_int64_exn s.address - info.code.end_ in
        Ok (placement.patches @ rest.patches, extend) 
      else
        let msg = "Not enough space to extend the code segment" in
        Error (Errors.No_patch_spaces msg) in
  let* new_ogre = match ogre with
    | None -> Ok None
    | Some ogre ->
      Log.send "Extending the OGRE specification";
      let* ogre = Extend_ogre.go ogre patches in
      Ok (Some ogre) in
  Log.send "Writing to patched binary %s" patched_binary;
  let* () = write info.code.addr patches binary patched_binary ~extend in
  Ok {patches; spaces = Spaces.of_list spaces; new_ogre}
