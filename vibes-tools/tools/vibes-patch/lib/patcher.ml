open Core
open Bap.Std
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

(* Copy the original binary and write the patches to it. *)
let patch_file
    (patches : patch list)
    (binary : string)
    (patched_binary : string) : unit =
  let orig_data = In_channel.read_all binary in
  Out_channel.with_file patched_binary ~f:(fun file ->
      Out_channel.output_string file orig_data;
      List.iter patches ~f:(fun patch ->
          Out_channel.seek file patch.loc;
          Out_channel.output_string file patch.data))

(* Get the raw binary data of the patch at the specified patch site. *)
let try_patch_site_aux
    (target : target)
    (asm : Asm.t)
    (language : T.language)
    (org : int64 option)
    (to_addr : int64 -> int64)
    (loc : int64)
    (overwritten : string list)
    (jmp : int64 option) : (string, KB.conflict) result =
  let module Target = (val target) in
  let asm' = Target.situate asm ~loc ~to_addr ~org ~jmp ~overwritten in
  Log.send "Situated assembly at 0x%Lx:\n%a" (to_addr loc) Asm.pp asm';
  let* objfile = Target.Toolchain.assemble asm' language in
  let* data = Target.Toolchain.to_binary objfile in
  (* If the origin was adjusted then shave off those bytes. *)
  let data = match org with
    | Some org -> String.drop_prefix data @@ Int64.to_int_exn org
    | None -> data in
  Ok data

(* Try to fit the patch into the specified patch site. *)
let try_patch_site
    ?(extern : bool = false)
    (region : Utils.region)
    (addr : int64)
    (size : int64)
    (ret : int64 option)
    (asm : Asm.t)
    (target : target)
    (language : T.language)
    (overwritten : string list) : (patch option, KB.conflict) result =
  let open Int64 in
  let module Target = (val target) in
  let loc = Utils.addr_to_offset addr region in
  let to_addr loc = Utils.offset_to_addr loc region in
  let org = Target.adjusted_org addr in
  let try_ =
    try_patch_site_aux target asm language
      org to_addr loc overwritten in
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
    (spaces : Patch_info.space list)
    (orig_region : Utils.region)
    (asm : Asm.t)
    (addr : int64)
    (size : int64)
    (spec : Ogre.doc)
    (target : target)
    (language : T.language)
    (o : overwrite) : ((patch * patch), KB.conflict) result =
  let module Target = (val target) in
  match spaces with
  | [] ->
    Error (Errors.No_patch_spaces "No suitable patch spaces found")
  | space :: rest ->
    let jumpto = Bitvec.to_int64 @@ Word.to_bitvec space.address in
    let next () =
      try_patch_spaces rest orig_region asm addr
        size spec target language o in
    Log.send "Attempting patch space 0x%Lx with %Ld bytes of space"
      jumpto space.size;
    match Utils.find_code_region jumpto spec with
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
        try_patch_site orig_region addr size None asm target language [] in
      match trampoline with
      | None ->
        Log.send "Trampoline doesn't fit";
        next ()
      | Some trampoline ->
        let len = String.length trampoline.data in
        Log.send "Trampoline fits (%d bytes)" len;
        let* overwritten, n =
          overwritten o Target.target addr Int64.(of_int len)
            ~zero:Int64.(size = 0L) in
        let ret = Int64.(addr + of_int n) in
        let* patch =
          try_patch_site region jumpto
            space.size (Some ret) asm target language
            overwritten ~extern:true in
        match patch with
        | Some patch -> Ok (patch, trampoline)
        | None -> next ()

(* Search for a space in the binary where the patch will fit. We first
   try the provided patch point, and if it doesn't work we will try the
   available external patch spaces that the user gave us. *)
let place_patch
    (o : overwrite)
    (patch_spaces : Patch_info.space list)
    (spec : Ogre.doc)
    (asm : Asm.t)
    (target : target)
    (language : T.language) : (patch * patch option, KB.conflict) result =
  let module Target = (val target) in
  let addr, size = asm.patch_point, asm.patch_size in
  Log.send "Attempting patch point 0x%Lx with %Ld bytes of space" addr size;
  let* region = match Utils.find_code_region addr spec with
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
      try_patch_site region addr size (Some ret) asm target language []
    else begin
      Log.send "Patch size is zero, need external patch space";
      Ok None
    end in
  match patch with
  | Some patch -> Ok (patch, None)
  | None ->
    let* patch, trampoline =
      try_patch_spaces patch_spaces region asm addr
        size spec target language o in
    Ok (patch, Some trampoline)

let consume_space
    (patch : patch)
    (spaces : Patch_info.space list) : Patch_info.space list =
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
  let* dis, info = target_info target language in
  let o = {dis; memmap} in
  Log.send "Solving patch placement";
  let rec place spaces acc = function
    | [] -> Ok (List.rev acc, spaces)
    | (asm : Asm.t) :: asms ->
      let* patch, trampoline = place_patch o spaces spec asm info language in
      Log.send "Solved patch placement: %a" pp_patch patch;
      Option.iter trampoline ~f:(Log.send "Trampoline: %a" pp_patch);
      let acc = match trampoline with
        | Some t -> t :: patch :: acc
        | None -> patch :: acc in
      let spaces = consume_space patch spaces in
      place spaces acc asms in
  let* patches, spaces = place (Spaces.to_list patch_spaces) [] asms in
  Log.send "Writing to patched binary %s" patched_binary;
  patch_file patches binary patched_binary;
  Ok (patches, Spaces.of_list spaces)
