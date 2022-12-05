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

type dis = (Dis.asm, Dis.empty) Dis.t

let (let*) x f = Result.bind x ~f

type spaces = Patch_info.space list
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
    else
      let msg = Format.asprintf "Unsupported target %a" T.Target.pp target in
      Error (Errors.Unsupported_target msg) in
  match Dis.lookup target language with
  | Ok dis -> Ok (Dis.store_asm dis, info)
  | Error err ->
    Error (Errors.No_disasm (Format.asprintf "%a" Error.pp err))

type overwrite = {
  dis : dis;
  memmap : value memmap;
}

type code_seg_alloc = {
  addr : int64;
  size : int64;
  end_ : int64;
  extra_space : int64;
}

let find_code_segment_alloc (spec : Ogre.doc) : code_seg_alloc option =
  let open Int64 in
  let (let*) x f = Option.bind x ~f in
  let query = Ogre.Query.(select @@ from Image.Scheme.segment) in
  let* segs = Ogre.eval (Ogre.collect query) spec |> Or_error.ok in
  let* code_addr, code_size = Seq.find_map segs ~f:(fun seg ->
      let {Image.Scheme.addr; size; info=(_,_,x)} = seg in
      Option.some_if x (addr, size)) in
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
  Log.send "Code segment end = 0x%Lx, \
            available space = %Ld bytes" code_end size;
  Some {
    addr = code_addr;
    size = code_size;
    end_ = code_end;
    extra_space = size;
  }

type info = {
  spec : Ogre.doc;
  target : target;
  language : T.language;
  o : overwrite;
  code : code_seg_alloc;
}

let within_extra_code (info : info) (addr : int64) : bool =
  let open Int64 in
  addr >= info.code.end_ &&
  addr < info.code.end_ + info.code.extra_space

let create_info
    (spec : Ogre.doc)
    (memmap : value memmap)
    (target : T.target)
    (language : T.language) : (info, KB.conflict) result =
  let* dis, target = target_info target language in
  match find_code_segment_alloc spec with
  | None -> Error (Errors.No_code_segment "No code segment found")
  | Some code -> Ok {spec; target; language; o = {dis; memmap}; code}

let find_mem
    (target : T.target)
    (patch_point : int64)
    (patch_size : int64)
    (memmap : value memmap) : (mem, KB.conflict) result =
  let width = T.Target.code_addr_size target in
  let from = Word.of_int64 ~width patch_point in
  Memmap.to_sequence memmap |> Seq.find_map ~f:(fun (mem, _) ->
      if Memory.contains mem from then
        Memory.view mem ~from |> Result.ok
      else None) |> function
  | Some mem -> Ok mem
  | None ->
    let msg = Format.asprintf
        "Couldn't find memory of at least size %Ld for patch point %a"
        patch_size Word.pp from in
    Error (Errors.Invalid_address msg)

let overwritten
    ?(zero : bool = false)
    (o : overwrite)
    (target : T.target)
    (patch_point : int64)
    (patch_size : int64) : (string list * int, KB.conflict) result =
  let invalid_size t =
    let msg = Format.asprintf
        "Invalid size %Ld for patch point 0x%Lx (%d bytes disassembled)"
        patch_size patch_point t in
    Error (Errors.Invalid_size msg) in
  let rec disasm acc mem t n = match Dis.insn_of_mem o.dis mem with
    | Error err ->
      let msg = Format.asprintf "Disasm error: %a" Error.pp err in
      Error (Errors.Invalid_insn msg)
    | Ok (_, None, _) ->
      (* The bytes at this address failed to decode to a valid instruction,
         but we should just assume that it's free space (i.e. inline data)
         and that the user intends to overwrite this region. *)
      Ok (List.rev acc, t)
    | Ok (m, Some insn, next) ->
      let len = Memory.length m in
      let n = n - len and t = t + len in
      let asm = Dis.Insn.asm insn in
      if n = 0 then Ok (List.rev (asm :: acc), t)
      else match next with
        | `finished -> invalid_size t
        | `left _ when n < 0 && not zero -> invalid_size t
        | `left _ when n <= 0 -> Ok (List.rev (asm :: acc), t)
        | `left mem -> disasm (asm :: acc) mem t n in
  let* mem = find_mem target patch_point patch_size o.memmap in
  let* insns, n = disasm [] mem 0 @@ Int64.to_int_exn patch_size in
  if not @@ List.is_empty insns then
    Log.send "The following instructions will be \
              overwritten at 0x%Lx:\n%s\n"
      patch_point @@ String.concat insns ~sep:"\n";
  Ok (insns, n)

type patch = {
  data : string;
  addr : int64;
  loc : int64;
}

let pp_patch (ppf : Format.formatter) (p : patch) : unit =
  Format.fprintf ppf "addr=0x%Lx, offset=0x%Lx, len=%d"
    p.addr p.loc @@ String.length p.data

type elf_seg = Elf.segment * int

let extend_elf_code_segment
    (elf : Elf.t)
    ((seg, i) : elf_seg)
    (data : bigstring)
    (size : int64) : unit =
  let open Int64 in
  let rd16le pos = Bigstring.get_int16_le data ~pos |> of_int in
  let rd16be pos = Bigstring.get_int16_be data ~pos |> of_int in
  let rd32le pos = Bigstring.get_int32_t_le data ~pos |> of_int32 in
  let rd32be pos = Bigstring.get_int32_t_be data ~pos |> of_int32 in
  let rd64le pos = Bigstring.get_int64_t_le data ~pos in
  let rd64be pos = Bigstring.get_int64_t_be data ~pos in
  let wr32le pos v = Bigstring.set_int32_t_le data ~pos @@ to_int32_trunc v in
  let wr32be pos v = Bigstring.set_int32_t_be data ~pos @@ to_int32_trunc v in
  let wr64le pos v = Bigstring.set_int64_t_le data ~pos v in
  let wr64be pos v = Bigstring.set_int64_t_be data ~pos v in
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
  wr (to_int_exn (entry + ms)) (seg.p_memsz + size)

let is_pt_load (seg : Elf.segment) : bool = match seg.p_type with
  | Elf.PT_LOAD -> true
  | _ -> false

let is_pf_exec (seg : Elf.segment) : bool =
  List.exists seg.p_flags ~f:(function
      | Elf.PF_X -> true
      | _ -> false)

let find_elf_segment
    (elf : Elf.t)
    (addr : int64) : (elf_seg, KB.conflict) result =
  Seq.find_mapi elf.e_segments ~f:(fun i seg ->
      if Int64.(seg.p_vaddr = addr)
      && is_pt_load seg && is_pf_exec seg
      then Some (seg, i) else None) |> function
  | Some x -> Ok x
  | None ->
    let msg = Format.sprintf
        "Segment at 0x%Lx was not found in the binary"
        addr in
    Error (Errors.Invalid_binary msg)

(* Copy the original binary and write the patches to it. *)
let patch_file
    ?(extend : int64 = 0L)
    (code_seg_addr : int64)
    (patches : patch list)
    (binary : string)
    (patched_binary : string) : (unit, KB.conflict) result =
  let data = Bigstring.of_string @@ In_channel.read_all binary in
  Log.send "Reading ELF headers";
  let* elf, seg = match Elf.from_bigstring data with
    | Error _ -> Error (Errors.Invalid_binary "Invalid ELF header")
    | Ok elf ->
      let* seg = find_elf_segment elf code_seg_addr in
      Ok (elf, seg) in
  Result.return @@ Out_channel.with_file patched_binary ~f:(fun file ->
      if Int64.(extend > 0L) then
        extend_elf_code_segment elf seg data extend;
      Out_channel.output_string file @@ Bigstring.to_string data;
      List.iter patches ~f:(fun patch ->
          Out_channel.seek file patch.loc;
          Out_channel.output_string file patch.data))

(* Get the raw binary data of the patch at the specified patch site. *)
let try_patch_site_aux
    (info : info)
    (asm : Asm.t)
    (org : int64 option)
    (to_addr : int64 -> int64)
    (loc : int64)
    (overwritten : string list)
    (jmp : int64 option) : (string, KB.conflict) result =
  let module Target = (val info.target) in
  let asm' = Target.situate asm ~loc ~to_addr ~org ~jmp ~overwritten in
  Log.send "Situated assembly at 0x%Lx:\n%a" (to_addr loc) Asm.pp asm';
  let* objfile = Target.Toolchain.assemble asm' info.language in
  let* data = Target.Toolchain.to_binary objfile in
  (* If the origin was adjusted then shave off those bytes. *)
  let data = match org with
    | Some org -> String.drop_prefix data @@ Int64.to_int_exn org
    | None -> data in
  Ok data

(* Try to fit the patch into the specified patch site. *)
let try_patch_site
    ?(extern : bool = false)
    (info : info)
    (region : Utils.region)
    (addr : int64)
    (size : int64)
    (ret : int64 option)
    (asm : Asm.t)
    (overwritten : string list) : (patch option, KB.conflict) result =
  let open Int64 in
  let module Target = (val info.target) in
  let loc = Utils.addr_to_offset addr region in
  let to_addr loc = Utils.offset_to_addr loc region in
  let org = Target.adjusted_org addr in
  let try_ = try_patch_site_aux info asm org to_addr loc overwritten in
  let end_jmp = Target.ends_in_jump asm in
  let inline_data = Target.has_inline_data asm in
  let needs_jmp = not end_jmp && (extern || inline_data) in
  let* data = try_ @@ match ret with
    | Some _ when needs_jmp -> ret
    | None when needs_jmp -> assert false
    | None | Some _ -> None in
  match of_int @@ String.length data with
  | len when len = size -> Ok (Some {data; addr; loc})
  | len when len < size && (end_jmp || needs_jmp) ->
    Ok (Some {data; addr; loc})
  | len when len < size ->
    (* For patching at the original location, we need to insert
       a jump to the end of the specified space. We need to check
       that the patch still fits because inserting a jump will
       increase the length of the patch. *)
    Log.send "Patch has %Ld bytes of space remaining, \
              requires a jump" (size - len);
    let* data = try_ @@ match ret with
      | None -> assert false
      | Some _ -> ret in
    Result.return begin match of_int @@ String.length data with
      | len when len <= size -> Some {data; addr; loc}
      | len ->
        Log.send "Patch doesn't fit at address 0x%Lx (%Ld bytes \
                  available, %Ld bytes needed)" addr size len;
        None
    end
  | len ->
    if Int64.(size = 0L) && end_jmp && not extern then
      (* Assume that a patch size of 0 means we can't overwrite
         anything, and that this patch is the trampoline to our
         external patch site. *)
      Ok (Some {data; addr; loc})
    else begin
      Log.send "Patch doesn't fit at address 0x%Lx (%Ld bytes \
                available, %Ld bytes needed)" addr size len;
      Ok None
    end

(* Try the provided external patch spaces. *)
let rec try_patch_spaces
    (info : info)
    (spaces : spaces)
    (orig_region : Utils.region)
    (asm : Asm.t)
    (addr : int64)
    (size : int64) : ((patch * patch), KB.conflict) result =
  let module Target = (val info.target) in
  match spaces with
  | [] ->
    Error (Errors.No_patch_spaces "No suitable patch spaces found")
  | space :: rest ->
    let jumpto = Bitvec.to_int64 @@ Word.to_bitvec space.address in
    let next () = try_patch_spaces info rest orig_region asm addr size in
    Log.send "Attempting patch space 0x%Lx with %Ld bytes of space"
      jumpto space.size;
    let region =
      if within_extra_code info jumpto then
        (* XXX: Is the code segment always at offset 0? *)
        Some Utils.{
            addr = info.code.end_;
            size = info.code.extra_space;
            offset = info.code.size;
          }
      else Utils.find_code_region jumpto info.spec in
    match region with
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
        let asm = Target.create_trampoline jumpto addr size in
        try_patch_site info orig_region addr size None asm [] in
      match trampoline with
      | None ->
        Log.send "Trampoline doesn't fit";
        next ()
      | Some trampoline ->
        let len = String.length trampoline.data in
        Log.send "Trampoline fits (%d bytes)" len;
        let* overwritten, n =
          overwritten info.o Target.target addr Int64.(of_int len)
            ~zero:Int64.(size = 0L) in
        let ret = Int64.(addr + of_int n) in
        let* patch =
          try_patch_site info region jumpto
            space.size (Some ret) asm
            overwritten ~extern:true in
        match patch with
        | Some patch -> Ok (patch, trampoline)
        | None -> next ()

(* Search for a space in the binary where the patch will fit. We first
   try the provided patch point, and if it doesn't work we will try the
   available external patch spaces that the user gave us. *)
let place_patch
    (info : info)
    (patch_spaces : spaces)
    (asm : Asm.t) : (patch * patch option, KB.conflict) result =
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
    if Int64.(size > 0L) then
      let ret = Int64.(addr + size) in
      try_patch_site info region addr size (Some ret) asm []
    else begin
      Log.send "Patch size is zero, need external patch space";
      Ok None
    end in
  match patch with
  | Some patch -> Ok (patch, None)
  | None ->
    let* patch, trampoline =
      try_patch_spaces info patch_spaces region asm addr size in
    Ok (patch, Some trampoline)

let consume_space (patch : patch) (spaces : spaces) : spaces =
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

type res = patch list * Spaces.t
type batch = patch list * spaces * Asm.t list

let rec place_and_consume
    ?(acc : patch list = [])
    (info : info)
    (spaces : spaces)
    (asms : Asm.t list) : (batch, KB.conflict) result =
  match asms with
  | [] -> Ok (List.rev acc, spaces, [])
  | (asm : Asm.t) :: rest ->
    match place_patch info spaces asm with
    | Error (Errors.No_patch_spaces _) ->
      Ok (List.rev acc, spaces, asms)
    | Error _ as err -> err
    | Ok (patch, trampoline) ->
      Log.send "Solved patch placement: %a" pp_patch patch;
      Option.iter trampoline ~f:(Log.send "Trampoline: %a" pp_patch);
      let acc = match trampoline with
        | Some t -> t :: patch :: acc
        | None -> patch :: acc in
      let spaces = consume_space patch spaces in
      place_and_consume info spaces rest ~acc

let patch
    ?(patch_spaces : Spaces.t = Spaces.empty)
    (target : T.target)
    (language : T.language)
    (asms : Asm.t list)
    ~(binary : string)
    ~(patched_binary : string) : (res, KB.conflict) result =
  Log.send "Loading binary %s" binary;
  let* image = Vibes_utils.Loader.image binary in
  let memmap = Image.memory image in
  let spec = Image.spec image in
  let* info = create_info spec memmap target language in
  Log.send "Solving patch placement";
  let spaces = Spaces.to_list patch_spaces in
  let had_spaces = not @@ List.is_empty spaces in
  let* patches, spaces, remaining = place_and_consume info spaces asms in
  let* patches, extend = match remaining with
    | [] -> Ok (patches, 0L)
    | _ ->
      if had_spaces
      then Log.send "Ran out of patch spaces, attempting to allocate"
      else Log.send "No patch spaces provided, attempting to allocate";
      let width = T.Target.code_addr_size target in
      let space = Patch_info.{
          address = Word.of_int64 ~width info.code.end_;
          size = info.code.extra_space;
        } in
      let* rest, spaces, asms = place_and_consume info [space] remaining in
      match asms with
      | _ :: _ ->
        Error (Errors.No_patch_spaces "Not enough space to extend \
                                       the code segment")
      | [] ->
        let remaining_space = List.hd_exn spaces in
        let size =
          let open Word in
          to_int64_exn (remaining_space.address - space.address) in
        Ok (patches @ rest, size) in
  let* () =
    Log.send "Writing to patched binary %s" patched_binary;
    patch_file info.code.addr patches binary patched_binary ~extend in
  Ok (patches, Spaces.of_list spaces)
