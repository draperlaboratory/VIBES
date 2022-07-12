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

(* Implements {!Vibes_plugin_parameters}. *)

open !Core_kernel
open Bap.Std
open Monads.Std

module Json = Yojson.Safe
module Vibes_config = Bap_vibes.Config
module Parse_c = Bap_vibes.Parse_c
module Hvar = Bap_vibes.Higher_var
module Wp_params = Bap_wp.Run_parameters
module Errors = Vibes_plugin_errors

(* Monadize the errors. *)
module Err = Monad.Result.Make (Errors) (Monad.Ident)
open Err.Syntax
type error = Errors.t Err.error

(* Get the default minizinc model filepath. *)
let minizinc_model_filepath = Vibes_plugin_constants.minizinc_model_filepath

(* Error if a string is empty. *)
let is_not_empty (value : string) (e : Errors.t)
  : (string, error) Stdlib.result =
  match String.length value with
  | 0 -> Err.fail e
  | _ -> Err.return value

(* Find the value of a field in a Json association list. *)
let value_of_field (field : string) (data : (string * Json.t) list)
  : Json.t option =
  match List.find data ~f:(fun (k, _) -> String.equal k field) with
  | Some (_, obj) -> Some obj
  | _ -> None

(* Validate a bitvector field in a Json association list. *)
let validate_bitvec_field (field : string) (data : (string * Json.t) list)
    (e : error) : (Bitvec.t, error) Stdlib.result =
  match value_of_field field data with
  | Some (`String s) ->
    begin
      try Err.return (Bitvec.of_string s)
      with Invalid_argument _ -> Err.fail e
    end
  | _ -> Err.fail e

(* Validate an int64 field in a Json association list. *)
let validate_int64_field (field : string) (data : (string * Json.t) list)
    (e : error) : (int64, error) Stdlib.result =
  match value_of_field field data with
  | Some (`String s) ->
    begin
      try Err.return (Int64.of_string s)
      with Invalid_argument _ -> Err.fail e
    end
  | _ -> Err.fail e

(* Validate a word field in a Json association list. *)
let validate_word_field (field : string) (data : (string * Json.t) list)
    (e : error) : (Word.t, error) Stdlib.result =
  match value_of_field field data with
  | Some (`String s) ->
    begin
      try Err.return (Word.of_string s)
      with Invalid_argument _ -> Err.fail e
    end
  | _ -> Err.fail e

(* Validate a string field in a Json association list. *)
let validate_string_field (field : string) (data : (string * Json.t) list)
    (e : error) : (string, error) Stdlib.result =
  match value_of_field field data with
  | Some (`String s) ->
    if (String.length s) > 0 then Err.return s
    else Err.fail e
  | _ -> Err.fail e

(* Extract the patch name and check it is non-empty string. *)
let validate_patch_name (obj : Json.t) : (string, error) Stdlib.result =
  match Json.Util.member "patch-name" obj with
  | `String s ->
    if String.length s = 0 then Err.fail Errors.Missing_patch_name
    else Err.return s
  | _ -> Err.fail Errors.Missing_patch_name

(* Extract the patch code, check it's a non-empty string, and parse
   into a [Cabs.definition] *)
let validate_patch_code (nm : string) (obj : Json.t)
  : (Vibes_config.patch_code, error) Stdlib.result =
  match Json.Util.member "patch-code" obj, Json.Util.member "asm-code" obj with
  | `String s, `Null ->
    (match Parse_c.parse_c_patch s with
     | Ok code -> Ok (Vibes_config.CCode code)
     | Error msg -> Error (Errors.Invalid_patch_code msg))
  | `Null, `String s -> Ok (Vibes_config.ASMCode s)
  | `String s, `String s' -> Error
      (Errors.Invalid_patch_code "Specified both assembly and C code in patch")
  | _, _ -> Err.fail Errors.Missing_patch_code

(* Extract the patch point field and parse the hex string into a bitvector, or
   error. *)
let validate_patch_point (obj : Json.t) : (Bitvec.t, error) Stdlib.result =
  match Json.Util.member "patch-point" obj with
  | `String s ->
    begin
      try
        Err.return (Bitvec.of_string s)
      with Invalid_argument _ ->
        let msg = Format.sprintf "Invalid hex string: %s" s in
        Err.fail (Errors.Invalid_hex msg)
    end
  | _ -> Err.fail Errors.Missing_patch_point

(* Extract the patch size integer, or error. *)
let validate_patch_size (obj : Json.t) : (int, error) Stdlib.result =
  match Json.Util.member "patch-size" obj with
  | `Int i -> Err.return i
  | _ -> Err.fail Errors.Missing_size

(* Extract the name of a higher variable, or error. *)
let validate_h_var_name (obj : Json.t) : (string, error) Stdlib.result =
  match Json.Util.member "name" obj with
  | `String s -> is_not_empty s (Errors.Missing_higher_var_name)
  | _ -> Err.fail Errors.Missing_higher_var_name

let validate_h_var_constant (addr : string)
  : (word, error) Stdlib.result =
  try Err.return @@ Word.of_string addr
  with Invalid_argument _ -> Err.fail Errors.Missing_higher_var_offset

let validate_h_var_memory
    (data : (string * Json.t) list) : (Hvar.memory, error) Stdlib.result =
  match value_of_field "address" data with
  | Some (`String addr) ->
    validate_h_var_constant addr >>| Hvar.create_global
  | Some _ -> Err.fail Errors.Missing_higher_var_offset
  | None ->
    validate_string_field
      "frame-pointer" data Errors.Missing_higher_var_fp >>= fun fp ->
    validate_word_field
      "offset" data Errors.Missing_higher_var_offset >>| fun offset ->
    Hvar.create_frame fp offset
    
(* Extract a higher variable, or error. *)
let validate_h_var (obj : Json.t) : (Hvar.t, error) Stdlib.result =
  validate_h_var_name obj >>= fun name ->
  match Json.Util.member "constant" obj with
  | `String const ->
    validate_h_var_constant const >>= fun const ->
    Err.return (Hvar.create_with_constant name ~const)
  | `Null -> begin
      match Json.Util.member "memory" obj with
      | `Assoc data ->
        validate_h_var_memory data >>| fun memory ->
        Hvar.create_with_memory name ~memory
      | `Null ->
        begin match Json.Util.member "at-entry" obj with
          | `Null -> Err.return None
          | `String reg -> Err.return @@ Some reg
          | _ -> Err.fail @@ Errors.Missing_higher_var_at_entry
        end >>= fun at_entry ->
        begin match Json.Util.member "at-exit" obj with
          | `Null -> Err.return None
          | `String reg -> Err.return @@ Some reg
          | _ -> Err.fail @@ Errors.Missing_higher_var_at_exit
        end >>= fun at_exit ->
        Err.return (Hvar.create_with_registers name ~at_entry ~at_exit)
      | _ -> Err.fail Errors.Missing_higher_var_offset
    end
  | _ -> Err.fail Errors.Missing_higher_var_offset

(* Extract the patch vars (which may be an empty list), or error. *)
let validate_patch_vars (obj : Json.t)
  : (Hvar.t list, error) Stdlib.result =
  match Json.Util.member "patch-vars" obj with
  | `List h_vars -> Err.all (List.map ~f:validate_h_var h_vars)
  | _ -> Err.return []

let validate_patch_sp_align (obj : Json.t)
  : (int, error) Stdlib.result =
  match Json.Util.member "patch-sp-align" obj with
  | `Int n -> Err.return n
  | `Null -> Err.return 0
  | _ -> Err.fail @@
    Errors.Invalid_sp_align "expected integer for patch-sp-align"

let validate_patch_extra_constraints (obj : Json.t)
  : (string option, error) Stdlib.result =
  match Json.Util.member "extra-constraints" obj with
  | `Null -> Err.return None
  | `String constraints -> Err.return (Some constraints)
  | _ -> Err.fail Errors.Invalid_extra_constraints

(* Validate a specific patch fragment within the list, or error *)
let validate_patch (obj : Json.t)
  : (Vibes_config.patch, error) Stdlib.result =
  validate_patch_name obj >>= fun patch_name ->
  validate_patch_code patch_name obj >>= fun patch_code ->
  validate_patch_point obj >>= fun patch_point ->
  validate_patch_size obj >>= fun patch_size ->
  validate_patch_vars obj >>= fun patch_vars ->
  validate_patch_sp_align obj >>= fun patch_sp_align ->
  validate_patch_extra_constraints obj >>= fun patch_extra_constraints ->
  let p = Vibes_config.create_patch
      ~patch_name ~patch_code ~patch_point ~patch_size ~patch_vars
      ~patch_sp_align ~patch_extra_constraints in
  Err.return p

(* Extract and validate the patch fragment list, or error. *)
let validate_patches (obj : Json.t)
  : (Vibes_config.patch list, error) Stdlib.result =
  match Json.Util.member "patches" obj with
  | `List ps -> Err.all (List.map ~f:validate_patch ps)
  | _ -> Err.fail Errors.Missing_patches

let validate_patch_space (obj : Json.t)
    : (Vibes_config.patch_space, error) Stdlib.result =
  let top_level_err = Errors.Invalid_patch_spaces
    "Each item in the patch-space list must be a JSON object."
  in
  let address_err = Errors.Invalid_patch_spaces
    "Error parsing `address` field, which must be a bitvector string."
  in
  let size_err = Errors.Invalid_patch_spaces
    "Error parsing `size` field, which must be an integer string."
  in
  match obj with
  | `Assoc data ->
     validate_int64_field "address" data address_err >>= fun address ->
     validate_int64_field "size" data size_err >>= fun size ->
     Err.return Vibes_config.{
       space_address = address;
       space_size = size;
     }
  | _ -> Err.fail top_level_err

(* Extract and validate the patch_space list, or error. *)
let validate_patch_spaces (obj : Json.t)
    : (Vibes_config.patch_space list, error) Stdlib.result =
  match Json.Util.member "patch-space" obj with
  | `List ps -> Err.all (List.map ~f:validate_patch_space ps)
  | `Null -> Err.return []
  | _ -> Err.fail (Errors.Invalid_patch_spaces "Must be a JSON list.")

(* Extract the property field string and parse it into an S-expression, or
   error. *)
let validate_wp_params (obj : Json.t)
  : (Wp_params.t, error) Stdlib.result =
  match Json.Util.member "wp-params" obj with
  | `Null -> begin
      match Json.Util.member "perform-verification" obj with
      | `Bool false ->
        Err.return @@ Wp_params.default ~func:"NO_VERIFICATION_PERFORMED"
      | _ -> Err.fail Errors.Missing_wp_params
    end
  | p ->
    begin
      let read s = Json.Util.member s p |> Json.Util.to_string_option in
      let triple l =
        match l with
        | [a; b; c] -> (a, b, c)
        | _ -> failwith "validate_wp_params: expected a ',' seperated triple!"
      in
      let func = read "func" |> Option.value ~default:"" in
      let precond = read "precond" |> Option.value ~default:"" in
      let postcond = read "postcond" |> Option.value ~default:"" in
      let user_func_specs_orig =
        read "user-func-specs-orig" |>
        Option.value_map ~default:[]
          ~f:(fun s ->
              String.split s ~on:';' |>
              List.map ~f:(String.split ~on:',') |>
              List.map ~f:triple)
      in
      let user_func_specs_mod =
        read "user-func-specs-mod" |>
        Option.value_map ~default:[]
          ~f:(fun s ->
              String.split s ~on:';' |>
              List.map ~f:(String.split ~on:',') |>
              List.map ~f:triple)
      in
      let inline = read "inline" in
      let fun_specs =
        read "fun-specs" |>
        Option.value_map ~default:[]
          ~f:(fun s -> String.split s ~on:',')
      in
      let ext_solver_path =
        read "ext-solver-path" |>
        Option.value_map ~default:(Some "boolector")
          ~f:(fun s -> if String.(s = "none") then None else Some s)
      in
      let show =
        read "show" |>
        Option.value_map ~default:[]
          ~f:(fun s ->
              String.split s ~on:',')
      in
      let use_fun_input_regs =
        read "use-fun-input-regs" |>
        Option.value_map ~default:false
          ~f:(fun s -> Bool.of_string s)
      in
      begin match Json.Util.member "init-mem" p with
        | `Null -> Err.return false
        | `Bool b -> Err.return b
        | _ -> Err.fail Errors.Invalid_init_mem
      end >>= fun init_mem ->
      if String.equal func "" then
        Err.fail Errors.Missing_func
      else
        let params = Wp_params.default ~func:func in
        Err.return
          { params with
            precond;
            postcond;
            user_func_specs_orig;
            user_func_specs_mod;
            inline;
            show;
            fun_specs;
            ext_solver_path;
            use_fun_input_regs;
            init_mem;
          }
    end

(* Extract the max-tries value, and make sure it's an [int] (if provided). *)
let validate_max_tries (obj : Json.t) : (int option, error) Stdlib.result =
  match Json.Util.member "max-tries" obj with
  | `Int i -> Err.return (Some i)
  | `Null -> Err.return None
  | _ -> Err.fail Errors.Invalid_max_tries

(* Extract the perform-verification value, and make sure it's a [bool] (if
   provided). Default to true.  *)
let validate_perform_verification (obj : Json.t) : (bool, error) Stdlib.result =
  match Json.Util.member "perform-verification" obj with
  | `Bool b -> Err.return b
  | `Null -> Err.return true
  | _ -> Err.fail Errors.Invalid_perform_verification

(* Extract the minizinc model filepathi value (or use the default), and make
   sure the file really exists. *)
let validate_minizinc_model_filepath (obj : Json.t)
  : (string, error) Stdlib.result =
  match Json.Util.member "minizinc-model" obj with
  | `String s ->
    begin
      if (String.length s) > 0 then
        begin
          let realpath = Vibes_plugin_utils.realpath s in
          match realpath with
          | Ok path -> Err.return path
          | Error e -> Err.fail e
        end
      else Err.fail (Errors.Missing_minizinc_model_filepath)
    end
  | _ ->
    begin
      let realpath = Vibes_plugin_utils.realpath minizinc_model_filepath in
      match realpath with
      | Ok path -> Err.return path
      | Error e -> Err.fail e
    end

let validate_minizinc_isel_filepath (obj : Json.t)
    : (string option, error) Stdlib.result =
  match Json.Util.member "minizinc-isel-filepath" obj with
  | `String s ->
    begin
      if (String.length s) > 0 then
        begin
          let realpath = Vibes_plugin_utils.realpath s in
          match realpath with
          | Ok path -> Err.return (Some path)
          | Error e -> Err.fail e
        end
      else Err.fail (Errors.Invalid_minizinc_isel_filepath)
    end
  | _ -> Ok None
let validate_ogre (obj : Json.t)
  : ((string * string) option, error) Stdlib.result =
  match Json.Util.member "ogre" obj with
  | `Null -> Err.return None
  | `String filename ->
    Err.return @@ Some (In_channel.read_all filename, "./" ^ filename)
  | _ -> Err.fail (Errors.Invalid_loader_data "must be a string.")

(* Parse the user-provided JSON config file into a Yojson.Safe.t *)
let parse_json (config_filepath : string) : (Json.t, error) Stdlib.result =
  try
    Err.return (Json.from_file config_filepath)
  with e -> Err.fail (Errors.Config_not_parsed (Exn.to_string e))

module BSI = struct

  (* Function information to pass to the loader. *)
  type func = {
    name : string;
    start : Bitvec.t;
    size : Bitvec.t;
    offset : Bitvec.t;
  }

  (* Metadata about the binary + functions to be loaded. *)
  type t = {
    arch : string;
    bits : int;
    is_little_endian : bool;
    entry_point : Bitvec.t;
    functions : func list;
  }

  let bitvec_of_int (i : int) : Bitvec.t =
    Bitvec.of_string @@ Int.to_string i

  let validate_address_range_start (name : string) (obj : Json.t)
    : (int, error) Stdlib.result =
    match Json.Util.member "start" obj with
    | `Int i -> Err.return i
    | `Null -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "missing field `start` for address range of function %s" name)
    | _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected integer for address range `start` \
                  of function %s" name)

  let validate_address_range_end (name : string) (obj : Json.t)
    : (int, error) Stdlib.result =
    match Json.Util.member "end" obj with
    | `Int i -> Err.return i
    | `Null -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "missing field `end` for address range of function %s" name)
    | _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected integer for address range `end` of function %s" name)

  let validate_address_ranges (name : string) (data : (string * Json.t) list)
    : (Bitvec.t * Bitvec.t, error) Stdlib.result =
    match value_of_field "address_ranges" data with
    | Some (`List data) -> begin
        (* XXX: consider how to load non-contiguous chunks with the same
           symbol name. *)
        match data with
        | x :: [] ->
          validate_address_range_start name x >>= fun start ->
          validate_address_range_end name x >>| fun end_ ->
          let size = bitvec_of_int @@ end_ - start in
          let start = bitvec_of_int start in
          start, size
        | _ -> Err.fail @@ Errors.Invalid_bsi_data
            (sprintf "expected singleton address range of function %s" name)
      end
    | Some _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected list for `address_ranges` of function %s" name)
    | None -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "couldn't find `address_ranges` of function %s" name)

  let validate_function_name (name : string) (data : (string * Json.t) list)
    : (string, error) Stdlib.result =
    match value_of_field "source_match" data with
    | Some (`Assoc data) -> begin
        match value_of_field "function" data with
        | Some (`String name) -> Err.return name
        | Some _ -> Err.fail @@ Errors.Invalid_bsi_data
            (sprintf "expected string for `function` of `source_match` \
                      of function %s" name)
        | None -> Err.fail @@ Errors.Invalid_bsi_data
            (sprintf "couldn't find `function` of `source_match` of \
                      function %s" name)
      end      
    | Some _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected dict for `source_match` of function %s" name)
    | None -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "couldn't find `source_match` of function %s" name)

  let validate_file_offset (name : string) (data : (string * Json.t) list)
    : (Bitvec.t, error) Stdlib.result =
    match value_of_field "file_offset" data with
    | Some (`Int i) -> Err.return @@ bitvec_of_int i
    | Some _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected integer for `file_offset` of function %s" name)
    | None -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "couldn't find `file_offset` of function %s" name)

  let validate_function ((name, obj) : string * Json.t)
    : (func, error) Stdlib.result =
    match obj with
    | `Assoc data ->
      validate_file_offset name data >>= fun offset ->
      validate_address_ranges name data >>= fun (start, size) ->
      validate_function_name name data >>| fun name ->
      {name; start; size; offset}
    | _ -> Err.fail @@ Errors.Invalid_bsi_data
        (sprintf "expected dict for function %s" name)

  let validate_functions (obj : Json.t)
    : (func list, error) Stdlib.result =
    match Json.Util.member "functions" obj with
    | `Assoc data -> begin
        Err.List.map data ~f:validate_function >>= function
        | [] -> Err.fail @@ Errors.Invalid_bsi_data
            "failed to parse any functions"
        | functions -> Err.return functions
      end
    | `Null -> Err.fail @@ Errors.Invalid_bsi_data
        "couldn't find `functions` in BSI metadata"
    | _ -> Err.fail @@ Errors.Invalid_bsi_data
        "`functions` field must be a dict"

  (* BAP may incorrectly assume that a Thumb binary is really an ARM binary,
     but we can correct that with a heuristic that checks the LSB of the
     entry point address.  *)
  let fix_arm_arch_string (entry_point : Bitvec.t) (arch : arch) : string =
    let lsb_set = Bitvec.(extract entry_point ~lo:0 ~hi:0 <> zero) in
    match arch with
    | #Arch.arm when lsb_set -> "thumb"
    | #Arch.armeb when lsb_set -> "thumbeb"
    | _ -> Arch.to_string arch

  let parse (exe : string) (obj : Json.t) : (t, error) Stdlib.result =
    match Image.create exe ~backend:"llvm" with
    | Error e -> Err.fail @@ Errors.Bad_image (exe, e)
    | Ok (image, _) -> validate_functions obj >>| fun functions ->
      let arch = Image.arch image in
      let bits = Size.in_bits @@ (Arch.addr_size arch :> size) in
      let is_little_endian = match Arch.endian arch with
        | LittleEndian -> true | BigEndian -> false in
      let entry_point = Image.entry_point image |> Addr.to_bitvec in
      let arch = fix_arm_arch_string entry_point arch in
      {arch; bits; is_little_endian; entry_point; functions}

  let to_ogre_string (bsi : t) : string =
    let preamble =
      sprintf
        "(declare arch (name str))\n\
         (declare bits (size int))\n\
         (declare base-address (addr int))\n\
         (declare entry-point (addr int))\n\
         (declare is-little-endian (flag bool))\n\
         (declare mapped (addr int) (size int) (off int))\n\
         (declare code-region (addr int) (size int) (off int))\n\
         (declare named-region (addr int) (size int) (name str))\n\
         (declare segment (addr int) (size int) (r bool) (w bool) (x bool))\n\
         (declare section (addr int) (size int))\n\
         (declare code-start (addr int))\n\
         (declare named-symbol (addr int) (name str))\n\
         (declare symbol-chunk (addr int) (size int) (root int))\n\n\
         (arch %s)\n\
         (bits %d)\n\
         (is-little-endian %s)\n\
         (base-address 0x0)\n\
         (entry-point %s)\n\n"
        bsi.arch bsi.bits
        (Bool.to_string bsi.is_little_endian)
        (Bitvec.to_string bsi.entry_point) in
    let functions =
      String.concat ~sep:"\n\n" @@
      List.map bsi.functions ~f:(fun f ->
          let start = Bitvec.to_string f.start in
          let size = Bitvec.to_string f.size in
          let offset = Bitvec.to_string f.offset in
          sprintf
            "(mapped %s %s %s)\n\
             (section %s %s)\n\
             (segment %s %s true false true)\n\
             (code-region %s %s %s)\n\
             (named-region %s %s %s)\n\
             (code-start %s)\n\
             (named-symbol %s %s)\n\
             (symbol-chunk %s %s %s)"
            start size offset
            start size
            start size
            start size offset
            start size f.name
            start
            start f.name
            start size start) in
    preamble ^ functions
  
end

let parse_bsi_metadata (exe : string) (config_json : Json.t)
  : (BSI.t option, error) Stdlib.result =
  match Json.Util.member "bsi-metadata" config_json with
  | `Null -> Err.return None
  | `String bsi_metadata_filepath ->
    parse_json bsi_metadata_filepath >>= BSI.parse exe >>| Option.return
  | _ -> Err.fail @@ Errors.Invalid_bsi_data
      "`bsi-metadata` field must be a string"

let validate_loader_info
    (bsi : BSI.t option)
    (config_json : Json.t)
  : ((string * string) option, error) Stdlib.result =
  validate_ogre config_json >>= fun ogre ->
  match bsi, ogre with
  | None, None -> Err.return None
  | None, Some ogre -> Err.return @@ Some ogre
  | Some bsi, None ->
    let str = BSI.to_ogre_string bsi in
    let filename, chan = Stdlib.Filename.open_temp_file "bsi" ".ogre" in
    Out_channel.fprintf chan "%s" str;
    Out_channel.flush chan;
    Out_channel.close chan;
    Err.return @@ Some (str, filename)
  | Some _, Some _ -> Err.fail Errors.Loader_data_conflict

(* Construct a configuration record from the given parameters. *)
let create
    ~exe:(exe : string)
    ~config_filepath:(config_filepath : string)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
  : (Vibes_config.t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  parse_json config_filepath >>= fun config_json ->
  parse_bsi_metadata exe config_json >>= fun bsi ->
  validate_patches config_json >>= fun patches ->
  validate_wp_params config_json >>= fun wp_params ->
  validate_max_tries config_json >>= fun max_tries ->
  validate_perform_verification config_json >>= fun perform_verification ->
  validate_loader_info bsi config_json >>= fun ogre ->
  (* Hand off the ogre filepath to WP if one was provided/generated. *)
  let wp_params, ogre = match ogre with
    | None -> wp_params, None
    | Some (ogre, filename) ->
      {wp_params with ogre = Some filename}, Some ogre in
  validate_minizinc_model_filepath config_json >>=
  fun minizinc_model_filepath ->
  validate_minizinc_isel_filepath config_json >>=
  fun minizinc_isel_filepath ->
  validate_patch_spaces config_json >>= fun patch_spaces ->
  let result = Vibes_config.create
    ~exe ~patches ~patched_exe_filepath ~max_tries ~perform_verification
    ~minizinc_model_filepath ~ogre ~patch_spaces ~wp_params
    ~minizinc_isel_filepath in
  Ok result
