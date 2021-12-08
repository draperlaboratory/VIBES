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

(* Extract where a higher variable is stored, or error. *)
let validate_h_var_stored_in (obj : Json.t) (field : string) (e : error)
    : (Hvar.stored_in, error) Stdlib.result =
  match Json.Util.member field obj with
  | `Assoc data ->
    begin
      match value_of_field "stored-in" data with
      | Some (`String "register") ->
        validate_string_field
          "register" data Errors.Missing_higher_var_reg >>= fun reg ->
        Err.return (Hvar.stored_in_register reg)
      | Some (`String "memory") ->
        begin
          match value_of_field "address" data with
          | Some (`String addr) ->
            begin
              try
                Err.return
                  Hvar.(stored_in_memory (create_global (Word.of_string addr)))
              with Invalid_argument _ ->
                Err.fail Errors.Missing_higher_var_offset
            end
          | Some _ -> Err.fail Errors.Missing_higher_var_offset
          | None ->
            validate_string_field
              "frame-pointer" data Errors.Missing_higher_var_fp >>= fun fp ->
            validate_word_field
              "offset" data Errors.Missing_higher_var_offset >>= fun offset ->
            Err.return Hvar.(stored_in_memory (create_frame fp offset))
        end
      | _ -> Err.fail Errors.Missing_higher_var_stored_in
    end
  | _ -> Err.fail e

(* Extract a higher variable, or error. *)
let validate_h_var (obj : Json.t) : (Hvar.t, error) Stdlib.result =
  validate_h_var_name obj >>= fun name ->
  match Json.Util.member "constant" obj with
  | `String addr ->
    validate_h_var_constant addr >>= fun addr ->
    Err.return (Hvar.create_with_constant name ~const:addr)
  | `Null ->
    validate_h_var_stored_in
      obj "at-entry" Errors.Missing_higher_var_at_entry >>= fun at_entry ->
    begin match Json.Util.member "at-exit" obj with
      | `Null -> Err.return None
      | _ ->
        validate_h_var_stored_in
          obj "at-exit" Errors.Missing_higher_var_at_exit >>| Option.return
    end >>= fun at_exit ->
    Err.return (Hvar.create_with_storage name ~at_entry ~at_exit)
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

(* Validate a specific patch fragment within the list, or error *)
let validate_patch (obj : Json.t)
    : (Vibes_config.patch, error) Stdlib.result =
  validate_patch_name obj >>= fun patch_name ->
  validate_patch_code patch_name obj >>= fun patch_code ->
  validate_patch_point obj >>= fun patch_point ->
  validate_patch_size obj >>= fun patch_size ->
  validate_patch_vars obj >>= fun patch_vars ->
  validate_patch_sp_align obj >>= fun patch_sp_align ->
  let p = Vibes_config.create_patch
      ~patch_name ~patch_code ~patch_point ~patch_size ~patch_vars
      ~patch_sp_align in
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
  let offset_err = Errors.Invalid_patch_spaces
    "Error parsing `offset` field, which must be a bitvector string."
  in
  let size_err = Errors.Invalid_patch_spaces
    "Error parsing `size` field, which must be an integer string."
  in
  match obj with
  | `Assoc data ->
     validate_int64_field "offset" data offset_err >>= fun offset ->
     validate_int64_field "size" data size_err >>= fun size ->
     Err.return { Vibes_config.space_offset = offset ;
                  Vibes_config.space_size = size }
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
  | `Null -> Err.fail Errors.Missing_wp_params
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
          }
    end

(* Extract the max-tries value, and make sure it's an [int] (if provided). *)
let validate_max_tries (obj : Json.t) : (int option, error) Stdlib.result =
  match Json.Util.member "max-tries" obj with
  | `Int i -> Err.return (Some i)
  | `Null -> Err.return None
  | _ -> Err.fail Errors.Invalid_max_tries

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

let validate_ogre (obj : Json.t)
  : (string option, error) Stdlib.result =
  match Json.Util.member "ogre" obj with
  | `Null -> Err.return None
  | `String filename -> Err.return (Some (In_channel.read_all filename))
  | _ -> Err.fail (Errors.Invalid_loader_data "must be a string.")

(* Parse the user-provided JSON config file into a Yojson.Safe.t *)
let parse_json (config_filepath : string) : (Json.t, error) Stdlib.result =
  try
    Err.return (Json.from_file config_filepath)
  with e -> Err.fail (Errors.Config_not_parsed (Exn.to_string e))

(* Construct a configuration record from the given parameters. *)
let create
    ~exe:(exe : string)
    ~config_filepath:(config_filepath : string)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    : (Vibes_config.t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  parse_json config_filepath >>= fun config_json ->
  validate_patches config_json >>= fun patches ->
  validate_wp_params config_json >>= fun wp_params ->
  validate_max_tries config_json >>= fun max_tries ->
  validate_ogre config_json >>= fun ogre ->
  validate_minizinc_model_filepath config_json >>=
    fun minizinc_model_filepath ->
  validate_patch_spaces config_json >>= fun patch_spaces ->
  let result = Vibes_config.create
    ~exe ~patches ~patched_exe_filepath ~max_tries
    ~minizinc_model_filepath ~ogre ~patch_spaces ~wp_params in
  Ok result
