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

(* Implements {!Patcher}. *)
open Core_kernel
open Bap_knowledge
open Bap_core_theory
open Bap.Std

module KB = Knowledge
module In = Core_kernel.In_channel
module Out = Core_kernel.Out_channel

type patch = {
  name : string;
  assembly : string list;
  orig_loc : int64;
  orig_size : int64;
} [@@deriving sexp, equal]

(* A [patch_site] is an extra location in the binary where it is
   acceptable to overwrite and place code.  *)
type patch_site = {
  location : int64;
  size : int64
} [@@deriving sexp]

(* A [placed_patch] is a patch that has a chosen location to place it
   in the binary.  It optionally may have a jump placed after it *)
type placed_patch = {
  assembly : string list;
  orig_loc : int64;
  orig_size : int64;
  patch_loc : int64;
  jmp : int64 option;
  org_offset : int option;
} [@@deriving sexp, equal]

let tgt_flag (l : Theory.language) : string =
  let l = Theory.Language.to_string l in
  let open String in
  if is_substring l ~substring:"arm" then ""
  else if is_substring l ~substring:"thumb" then "-mthumb"
  (* Needed for testing *)
  else if is_substring l ~substring:"unknown" then ""
  else failwith ("Unsupported language: " ^ l)

(* If the assembler inserted a literal pool, then get its size. *)
let size_of_literal_pool (l : Theory.language) (filename : string) : int64 =
  let l = Theory.Language.to_string l in
  let is_arm = String.is_substring l ~substring:"arm" in
  let is_thumb = String.is_substring l ~substring:"thumb" in
  if not (is_arm || is_thumb) then 0L
  else
    let cmd query =
      let command =
        Printf.sprintf
          "objdump %s -d | grep \"%s\" | wc -l"
          filename query in
      let in_channel = Caml_unix.open_process_in command in
      match In_channel.input_line in_channel with
      | None -> 0L
      | Some count -> Int64.of_string count in
    (* Find evidence of using constant pools. *)
    match cmd "ldr.*, \\[pc, .*\\]" with
    | 0L -> 0L
    | _ ->
      let thumb_padding =
        (* Padding may have been inserted on Thumb to avoid unaligned
           access. This never happens on ARM since everything ends up
           4 byte-aligned. *)
        if is_thumb then Int64.(cmd "\\.short" * 2L) else 0L in
      let consts = Int64.(cmd "\\.word" * 4L) in
      Int64.(thumb_padding + consts)

(* Check if a literal pool was inserted at the end of our patch. *)
let check_for_literal_pool (l : Theory.language) (assembly : string list)
  : (int64 * string, Kb_error.t) result =
  let (let*) x f = Result.bind x ~f in
  (* Write assembly to temporary file *)
  let asm_filename = Stdlib.Filename.temp_file "vibes-assembly" ".asm" in
  Out.write_lines asm_filename assembly;
  (* run assembler *)
  let assembler = "/usr/bin/arm-linux-gnueabi-as" in
  let with_elf_filename = Stdlib.Filename.temp_file "vibes-assembly" ".o" in
  (* FIXME: a bit hacky, we should have a dictionary here probably. *)
  let tgt_flag = tgt_flag l in
  let args =
    [
      "-o";
      with_elf_filename;
      tgt_flag;
      asm_filename
    ]
  in
  let info_str =
    Format.asprintf "Calling %s %s..."
      assembler
      (String.concat ~sep:" " args)
  in
  Events.(send @@ Info info_str);
  let* _ = Utils.run_process assembler args in
  let literal = size_of_literal_pool l with_elf_filename in
  Result.return (literal, with_elf_filename)

(** [binary_of_elf] uses external programs to convert assembly code to binary *)
let binary_of_elf (with_elf_filename : string) : (string, Kb_error.t) Result.t =
  let (let*) x f = Result.bind x ~f in
  (* strip elf data *)
  let objcopy = "/usr/bin/arm-linux-gnueabi-objcopy" in
  let raw_bin_filename = Stdlib.Filename.temp_file "vibes-assembly" ".bin" in
  let objcopy_args =
    [
      "-O";
      "binary";
      with_elf_filename;
      raw_bin_filename
    ]
  in
  let* _ = Utils.run_process objcopy objcopy_args in
  let patch_exe = In.read_all raw_bin_filename in
  Ok patch_exe

(**

   [jmp_instr_size] is the implicit size in bytes of the code for an
   unconditional branch instruction We may need to generalize this
   number at some point for different architectures and different
   kinds of jumps.

*)
let jmp_instr_size : int64 = 4L


(** [build_patch] returns the binary of a patch with athe appropriate jumps *)
let build_patch
    (l : Theory.language)
    (patch : placed_patch)
  : (int64 * string, Kb_error.t) Result.t =
  let (let*) x f = Result.bind x ~f in
  (* [abs_jmp] produces assembly for an unconditional jmp *)
  let abs_jmp (abs_addr : int64) : string =
    Printf.sprintf "b (%s + (%Ld))" Constants.patch_start_label abs_addr in
  let patch_relative = Printf.sprintf ".equiv %s, %Ld\n"
      Constants.relative_patch_placement
      Int64.(patch.patch_loc - patch.orig_loc)
  in
  let patch_jmp = match patch.jmp with
    | None -> ""
    | Some j -> abs_jmp Int64.(j - patch.patch_loc)
  in
  let patch_loc = Printf.sprintf ".equiv %s, %Ld\n"
      Constants.patch_location patch.patch_loc in
  let patch_start = Printf.sprintf "%s:" Constants.patch_start_label in
  let org = match patch.org_offset with
    | Some off -> Printf.sprintf ".org %d" off
    | None -> "" in
  let asm =
    ".syntax unified\n" ::
    patch_loc ::
    patch_relative ::
    org ::
    patch_start ::
    (patch.assembly @ [patch_jmp]) in
  let* literal, objfile = check_for_literal_pool l asm in
  let* bin = binary_of_elf objfile in
  Result.return (literal, bin)

let patch_size (l : Theory.language) (patch : patch)
  : (int64 * int64, Kb_error.t) Result.t =
  let placed_patch = {
    assembly = patch.assembly;
    orig_loc = patch.orig_loc;
    orig_size = patch.orig_size;
    patch_loc = patch.orig_loc;
    jmp = None;
    org_offset = None;
  } in
  build_patch l placed_patch |>
  Result.map ~f:(fun (literal, b) ->
      String.length b |> Int64.of_int, literal)

(** [patch_file] takes a filename and a list of patch binaries and
    locations and returns the filename of a patched file *)
let patch_file (lang : Theory.language)
    ~filename:(original_exe_filename : string)
    (patches : placed_patch list)
  : string =
  let tmp_patched_exe_filename =
    Stdlib.Filename.temp_file "vibes-assembly" ".patched" in
  let orig_exe = In.read_all original_exe_filename in
  Out.with_file tmp_patched_exe_filename
    ~f:(fun file ->
        Out.output_string file orig_exe;
        Core_kernel.List.iter patches
          ~f:(fun patch ->
              Out.seek file patch.patch_loc;
              let patch_binary =
                build_patch lang patch |>
                Result.map_error ~f:(Format.asprintf "%a" Kb_error.pp) |>
                Result.ok_or_failwith |> snd in 
              (* Shave off the extra bytes at the beginning if we shifted
                 the patch origin. *)
              Option.value_map patch.org_offset ~default:patch_binary
                ~f:(String.drop_prefix patch_binary) |>
              Out.output_string file
            ));
  tmp_patched_exe_filename


(** [naive_find_patch_sites] finds a region called "vibes_dummy" and
    returns a [patch_site] associated with it.

    TODO: Extend to other named regions, and use the Bap knowledge
    base. *)
let naive_find_patch_sites (filename : string) : patch_site list =
  (* TODO: Surely there must be a better way *)
  let command =
    Printf.sprintf
      "objdump %s -dF 2>&1 | grep vibes_dummy | grep -oP \"(?<=File Offset: 0x)([0-9a-fA-F]+)\""
      filename
  in
  let in_channel = Caml_unix.open_process_in command in
  match In_channel.input_line in_channel with
  | None -> []
  | Some addr_string ->
    let location = Scanf.sscanf addr_string "%Lx" (fun i -> i) in
    [{location; size = 128L}]

(** [exact_fit_patch] builds a placed_patch that fits exactly in the
    old location *)
let exact_fit_patch ?(org_offset : int option = None)
    (patch : patch) : placed_patch = {
  assembly = patch.assembly;
  orig_loc = patch.orig_loc;
  orig_size = patch.orig_size;
  patch_loc = patch.orig_loc;
  jmp = None;
  org_offset;
}

(** [loose_fit_patch] builds a placed_patch that fits loosely in the
    old location and hence needs an extra jump placed. This also
    returns the remainder of the space as a [patch_site] for possible
    further patch placement *)
let loose_fit_patch ?(org_offset : int option = None)
    (patch : patch) (patch_size : int64) : placed_patch * patch_site =
  let open Int64 in
  let patch_site = {
    location = patch.orig_loc + patch_size + jmp_instr_size;
    size = patch.orig_size - jmp_instr_size - patch_size
  } in
  let placed_patch = {
    (exact_fit_patch patch ~org_offset) with
    jmp = Some Int64.(patch.orig_loc + patch.orig_size)
  } in
  (placed_patch, patch_site)

(** [external_patch_site] places a patch at an external [patch_loc]
    location. This function returns two [placed_patch]. The first is
    the jump to the patch that goes where the original code was.  The
    second is the patch itself. *)
let external_patch_site
    (patch : patch)
    (patch_loc : int64) : placed_patch * placed_patch =
  let open Int64 in
  let placed_patch = exact_fit_patch patch in
  let jmp_to_patch = {
    placed_patch with
    assembly = [];
    jmp = Some patch_loc
  } in
  let placed_patch = {
    placed_patch with
    patch_loc = patch_loc;
    jmp = Some (patch.orig_size + patch.orig_loc)
  } in
  (jmp_to_patch, placed_patch)

(** [find_site_greedy] goes through a [patch_site] list and finds the
   first one in which a patch of size [patch_size] can fit. It then
   returns the address of this site and a modified [patch_site] list
   with those locations removed.*)
let find_site_greedy (patch_sites : patch_site list) (patch_size : int64)
  : int64 * patch_site list =
  let open Int64 in
  let rec find_site_aux patch_sites =
    match patch_sites with
    (* FIXME: fail more gracefully here *)
    | [] -> failwith @@
      sprintf "Couldn't fit patch anywhere (%Ld bytes long)" patch_size
    | p :: ps -> if p.size >= patch_size
      then
        let new_patch_site = {
          location = p.location + patch_size;
          size = p.size - patch_size
        } in
        (p.location, new_patch_site :: ps)
      else
        let (loc, ps) = find_site_aux ps in
        (loc, p :: ps)
  in
  find_site_aux patch_sites

(** [place_patches] given patches and patch_sites, find a way to split and pack patches
    At the moment it is just greedy.
*)
let place_patches
    (tgt : Theory.target)
    (lang : Theory.language)
    (patches : patch list)
    (patch_sites : patch_site list) : placed_patch list =
  let open Int64 in
  let align = (Theory.Target.code_alignment tgt |> of_int) lsr 3 in
  let process_patch (acc, patch_sites) patch =
    let patch_size, literal =
      patch_size lang patch |> Result.ok |> Option.value_exn in
    Events.send @@ Info (
      sprintf "Patch %s, size = %Ld\n%!"
        patch.name Int64.(literal + patch_size));
    let has_literal = literal <> 0L in
    let org_offset =
      (* If we have a literal pool in the patch, then objcopy may insert extra
         padding if it determines that an unaligned memory access is possible. 
         We need to anticipate this by telling the assembler that our patch will
         be at some offset from the origin. We will then fix up the code once
         we actually insert the patch into the new binary.

         Consider the following cases in a Thumb binary (ARM doesn't apply to
         this problem since everything is always aligned by the same boundary):

         1) Our patch is inserted at an address that is aligned by 2 (but not 4):

            a) If the patch ends on an address that is aligned by 4, then the
               size of our patch has a remainder of 2. Therefore, padding will
               be inserted between our patch code and the literal pool. Since
               the start of our patch is misaligned, we need to adjust the
               origin such that all PC-relative offsets end up the same as if
               our patch was inserted at an aligned boundary.

            b) Otherwise, the patch ends on an address that is aligned by 2,
               so the assembler will incorrectly insert padding between our
               patch and the literal pool. Therefore, we need to adjust the
               origin of our patch for the assembler.

         2) Our patch is inserted at an address that is aligned by 4. Whether
            or not padding is inserted by the assembler, we have a relative
            starting position of 0, so objcopy won't insert padding behind our
            backs, and thus our PC-relative offsets remain consistent.
      *)
      if has_literal then
        (* Is the patch location aligned? *)
        let align_loc = rem patch.orig_loc align in
        if align_loc <> 0L then Some (to_int_exn align_loc) else None
      else None in
    (* If a literal pool was inserted at the end, then we need to insert
       a jump. *)
    if has_literal then
      Events.send @@ Info "Found literal pool at end of patch.";
    let patch_size =
      if has_literal then patch_size + jmp_instr_size else patch_size in
    if patch_size <= patch.orig_size (* Patch fits inplace *)
    then
      (* If patch exactly fits *)
      if patch_size = patch.orig_size then
        let exact = exact_fit_patch patch ~org_offset in
        let exact = if not has_literal then exact else {
            exact with
            jmp = Some Int64.(patch.orig_loc + patch.orig_size)
          } in
        (exact :: acc, patch_sites)
      else (* Inexact fit. Add jmp, put leftover space in patch_sites *)
        let placed_patch, new_patch_site =
          loose_fit_patch patch (patch_size - jmp_instr_size) ~org_offset in
        (placed_patch :: acc, new_patch_site :: patch_sites)
    else (* Patch does not fit inplace*)
      (* Find patch_site that works *)
      let patch_size =
        if not has_literal then patch_size + jmp_instr_size else patch_size in
      let (patch_loc, patch_sites) = find_site_greedy patch_sites patch_size in
      let (jmp_to_patch, placed_patch) = external_patch_site patch patch_loc in
      (* TODO: We could also insert the unused original space into
         patch_sites here. *)
      (jmp_to_patch :: placed_patch  :: acc , patch_sites)
  in
  let (placed_patches, _) =
    List.fold patches
      ~init:([], patch_sites)
      ~f:process_patch
  in
  placed_patches


(* A datatype that encodes the code region that shall contain the placed patch.
   Usually computed by an invocation to ogre on the [Image.Scheme.code_region]
   associated to the code unit.
*)
type patch_region = { region_addr : int64; region_offset : int64 }

let ogre_compute_region ~loc:(patch_point : int64) (spec : Ogre.doc) : patch_region Or_error.t =
  let code_region =
    Ogre.eval
      (Ogre.require
         ~that:Int64.(fun (addr,size,_) ->
             addr <= patch_point && patch_point <= addr + size)
   Image.Scheme.code_region) spec
  in
  Or_error.map code_region
    ~f:(fun (addr, _size, offset) ->
        { region_addr = addr; region_offset = offset })


(** [reify_patch] gets out of the knowledge base all the information to fill the
    [patch] data type. It performs some translation of address space numbers to
    file offsets.
    See https://gitter.im/BinaryAnalysisPlatform/vibes?at=6011cca5aa6a6f319de9381d
    for more discussion of this.
*)
let reify_patch
    ~compute_region:(compute_region)
    ~exe_unit:(exe_unit)
    (patch : Data.Patch.t) : patch KB.t =
  let open KB.Let in
  let* name = Data.Patch.get_patch_name_exn patch in
  let* patch_point = Data.Patch.get_patch_point_exn patch in
  let* addr = Theory.Label.for_addr patch_point in
  let* unit = KB.collect Theory.Label.unit addr in
  let* unit = match unit with
    | None -> KB.return exe_unit
    | Some x -> KB.return x
  in
  let* spec = KB.collect Image.Spec.slot unit in
  let patch_point = Bitvec.to_int64 patch_point in
  let patch_region = compute_region ~loc:patch_point spec in
  let* {region_addr; region_offset} = match patch_region with
  | Error s -> Kb_error.fail (Kb_error.Other (Core_kernel.Error.to_string_hum s))
  | Ok c -> KB.return c
  in
  (* The distance of patch address from region start address is calculated
     and then added to the region file offset to get the patch file offset *)
  let patch_file_offset = Int64.(patch_point - region_addr + region_offset) in
  let* patch_size = Data.Patch.get_patch_size_exn patch in
  let* assembly = Data.Patch.get_assembly_exn patch in
  KB.return {
    name;
    assembly;
    orig_loc = patch_file_offset;
    orig_size = Int64.of_int patch_size;
  }

(* Turn a KB patch space into a patch site (the type used by this module to
   represent the same information *)
let reify_patch_site (obj : Data.Patch_space.t) : patch_site KB.t =
  let open KB.Let in
  let* location = Data.Patch_space.get_offset_exn obj in
  let* size = Data.Patch_space.get_size_exn obj in
  KB.return {location; size}

(* Get the patch_spaces out of the kb monad and into the form used by this
   module *)
let reify_patch_sites (obj : Data.t) : patch_site list KB.t =
  let open KB.Let in
  let* patch_space_set : Data.Patch_space_set.t =
    Data.Original_exe.get_patch_spaces obj
  in
  let patch_spaces : Data.Patch_space.t list =
    Data.Patch_space_set.to_list patch_space_set
  in
  KB.List.map ~f:reify_patch_site patch_spaces

(* Patches the original exe, to produce a patched exe. *)
let patch
    ?compute_region:(compute_region=ogre_compute_region)
    ?patcher:(patcher=patch_file)
    (obj : Data.t) : unit KB.t =
  let open KB.Let in
  Events.(send @@ Header "Starting patcher");
  (* Get patch information (the address to start patching and the number
     of bytes to overwrite), and get the patch assembly. *)
  Events.(send @@ Info "Retrieving data from KB...");

  let* original_exe_filename = Data.Original_exe.get_filepath_exn obj in
  let* original_exe_unit = Theory.Unit.for_file original_exe_filename in
  let* patches = Data.Patched_exe.get_patches obj in
  let patch_list = Data.Patch_set.to_list patches in
  let reify_patch = reify_patch
    ~compute_region:compute_region ~exe_unit:original_exe_unit in
  let* patch_list = KB.List.map ~f:reify_patch patch_list in
  let naive_patch_sites = naive_find_patch_sites original_exe_filename in
  let* provided_patch_sites = reify_patch_sites obj in
  let patch_sites = naive_patch_sites @ provided_patch_sites in
  let* target =
    let patch = Data.Patch_set.choose_exn patches in
    Data.Patch.get_target patch in
  let* lang =
    let patch = Data.Patch_set.choose_exn patches in
    Data.Patch.get_lang patch in
  Events.(send @@ Info "Found Patch Sites:");
  Events.(send @@ Info (Format.asprintf "%a" Sexp.pp_hum @@ sexp_of_list sexp_of_patch_site patch_sites));
  Events.(send @@ Info "Solving patch placement...");
  let placed_patches = place_patches target lang patch_list patch_sites in
  Events.(send @@ Info "Patch Placement Solution:");
  Events.(send @@ Info (Format.asprintf "%a" Sexp.pp_hum @@ sexp_of_list sexp_of_placed_patch placed_patches));
  Events.(send @@ Info "Patching file...");
  let tmp_patched_exe_filename = patcher lang ~filename:original_exe_filename placed_patches in

  (* Stash the filepath in the KB. *)
  let* _ = Data.Patched_exe.set_tmp_filepath obj (Some tmp_patched_exe_filename) in
  KB.return ()
