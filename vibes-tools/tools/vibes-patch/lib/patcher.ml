open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Constants = Vibes_constants.Asm
module Patch_info = Vibes_patch_info.Types
module Asm = Vibes_as.Types.Assembly
module Log = Vibes_log.Stream

let (let*) x f = Result.bind x ~f

type target = (module Types.Target)

let target_info (target : T.target) : (target, KB.conflict) result =
  if T.Target.belongs Arm_target.parent target then
    Ok (module Arm_utils)
  else
    let msg = Format.asprintf "Unsupported target %a" T.Target.pp target in
    Error (Errors.Unsupported_target msg)

type patch = {
  data : string;
  loc : int64;
  len : int64;
} [@@deriving sexp]

(* Copy the original binary and write the patches to it. *)
let patch_file
    ?(trampoline : patch option = None)
    (patch : patch)
    (binary : string)
    (patched_binary : string) : unit =
  let orig_data = In_channel.read_all binary in
  Out_channel.with_file patched_binary ~f:(fun file ->
      Out_channel.output_string file orig_data;
      Out_channel.seek file patch.loc;
      Out_channel.output_string file patch.data;
      Option.iter trampoline ~f:(fun {loc; data; _} ->
          Out_channel.seek file loc;
          Out_channel.output_string file data))

(* Get the raw binary data of the patch at the specified patch site. *)
let try_patch_site_aux
    (target : target)
    (asm : Asm.t)
    (language : T.language)
    (org : int64 option)
    (to_addr : int64 -> int64)
    (loc : int64)
    (jmp : int64 option) : (string, KB.conflict) result =
  let module Target = (val target) in
  let asm' = Target.situate asm ~loc ~to_addr ~org ~jmp in
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
    (language : T.language) : (patch option, KB.conflict) result =
  let module Target = (val target) in
  let loc = Utils.addr_to_offset addr region in
  let to_addr loc = Utils.offset_to_addr loc region in
  let org = Target.adjusted_org addr in
  let try_ = try_patch_site_aux target asm language org to_addr loc in
  let jmp = match ret with
    | Some _ when extern || Target.has_inline_data asm -> ret
    | None when extern -> assert false
    | None | Some _ -> None in
  let* data = try_ jmp in
  let len = Int64.of_int @@ String.length data in
  if Int64.(len = size) then
    Ok (Some {data; loc; len})
  else if Int64.(len < size) then
    if extern || Option.is_some jmp then
      Ok (Some {data; loc; len})
    else
      (* For patching at the original location, we need to insert
         a jump to the end of the specified space. *)
      let* data = try_ ret in
      let len = Int64.of_int @@ String.length data in
      Ok (Option.some_if Int64.(len <= size) {data; loc; len})
  else Ok None

(* Try the provided external patch spaces. *)
let rec try_patch_spaces
    (spaces : Patch_info.space list)
    (orig_region : Utils.region)
    (asm : Asm.t)
    (addr : int64)
    (size : int64)
    (ret : int64)
    (spec : Ogre.doc)
    (target : target)
    (language : T.language) : ((patch * patch), KB.conflict) result =
  let module Target = (val target) in
  let open Patch_info in
  match spaces with
  | [] ->
    Error (Errors.No_patch_spaces "No suitable patch spaces found")
  | space :: rest ->
    let* region = Utils.find_code_region space.address spec in
    let trampoline =
      Utils.addr_to_offset space.address region |>
      Target.create_trampoline in
    (* We have to make sure that the jump to the external space
       will fit at the intended patch point. *)
    let* trampoline =
      try_patch_site orig_region addr size
        None trampoline target language in
    match trampoline with
    | None ->
      Error (Errors.No_patch_room
               "No room for trampoline to \
                external patch space")
    | Some trampoline ->
      let* patch =
        try_patch_site region space.address
          space.size (Some ret) asm target language
          ~extern:true in
      match patch with
      | Some patch -> Ok (patch, trampoline)
      | None ->
        try_patch_spaces rest orig_region asm
          addr size ret spec target language

(* Attempt to place the patch. *)
let place_patch
    (patch_info : Patch_info.t)
    (spec : Ogre.doc)
    (asm : Asm.t)
    (target : target)
    (language : T.language) : (patch * patch option, KB.conflict) result =
  let open Patch_info in
  let module Target = (val target) in
  let addr, size =
    Word.to_int64_exn patch_info.patch_point,
    patch_info.patch_size in
  let* region = Utils.find_code_region addr spec in
  let ret = Utils.addr_to_offset Int64.(addr + size) region in
  let* patch =
    try_patch_site region addr size (Some ret) asm target language in
  match patch with
  | Some patch -> Ok (patch, None)
  | None ->
    let* patch, trampoline =
      try_patch_spaces patch_info.patch_spaces
        region asm addr size ret spec target language in
    Ok (patch, Some trampoline)

let patch
    (patch_info : Patch_info.t)
    (target : T.target)
    (language : T.language)
    (asm : Asm.t)
    ~(binary : string)
    ~(patched_binary : string) : (unit, KB.conflict) result =
  Log.send "Loading binary %s" binary;
  let* image = Loader.image binary in
  let spec = Image.spec image in
  let* target = target_info target in
  Log.send "Solving patch placement";
  let* patch, trampoline = place_patch patch_info spec asm target language in
  Log.send "Solved patch placement: %a" Sexp.pp_hum @@ sexp_of_patch patch;
  Option.map trampoline ~f:sexp_of_patch |>
  Option.iter ~f:(Log.send "Trampoline: %a" Sexp.pp_hum);
  Log.send "Writing to patched binary %s" patched_binary;
  patch_file patch binary patched_binary ~trampoline;
  Ok ()
