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

(* Implements {!Config}. *)

open !Core_kernel

module Hvar = Higher_var
module Wp_params = Bap_wp.Run_parameters

type patch_code = CCode of Cabs.definition | ASMCode of string

(* A type to represent a patch. *)
type patch =
  {
    (* The name of the patch to use. *)
    patch_name : string;

    (* The C AST produced by FrontC. *)
    patch_code : patch_code;

    (* The address in the original exe to start patching from. *)
    patch_point : Bitvec.t;

    (* The number of bytes of code that the patch replaces or removes,
       beginning at the patch_point *)
    patch_size : int;

    (* Higher variables *)
    patch_vars : Hvar.t list;

    (* The amount that the SP should be adjusted by to be aligned
       at the beginning of the patch (for ABI reasons). *)
    patch_sp_align : int;

    (* Optional extra constraints to inject into minzinc *)
    patch_extra_constraints : string option
  }

(** A type to represent known regions that may be overwritten with patch code *)
type patch_space = {
    space_address : int64;
    space_size : int64
  }

type ogre_spec = (string, string option * string option) Either.t

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string; (* The filename (path) of the executable to patch. *)
  patches : patch list; (* The list of patches to apply. *)
  patched_exe_filepath : string option; (* Optional output location *)
  max_tries : int option; (* Optional number of CEGIS iterations to allow *)
  perform_verification : bool; (* Whether to verify *)
  minizinc_model_filepath : string; (* Path to a minizinc model file *)
  minizinc_isel_filepath : string option;
  ogre : ogre_spec option;
  patch_spaces : patch_space list;
  wp_params : Wp_params.t;
}

(* Patch accessors. *)
let patch_name (p : patch) : string = p.patch_name
let patch_code (p : patch) : patch_code = p.patch_code
let patch_point (p : patch) : Bitvec.t = p.patch_point
let patch_size (p : patch) : int = p.patch_size
let patch_vars (p : patch) : Hvar.t list = p.patch_vars
let patch_sp_align (p : patch) : int = p.patch_sp_align
let patch_extra_constraints (p : patch) : string option = p.patch_extra_constraints

(* Config accessors. *)
let exe t : string = t.exe
let patches t : patch list = t.patches
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries
let perform_verification t : bool = t.perform_verification
let minizinc_model_filepath t : string = t.minizinc_model_filepath
let minizinc_isel_filepath t : string option = t.minizinc_isel_filepath
let ogre t : ogre_spec option = t.ogre
let patch_spaces t : patch_space list = t.patch_spaces
let wp_params t : Wp_params.t = t.wp_params

(* For displaying a higher var. *)
let string_of_hvar (v : Hvar.t) : string =
  let string_of_value (name : string) (v : Hvar.value) : string =
    match v with
    | Registers {at_entry = Some x; at_exit = Some y} -> 
      Format.sprintf
        "      {\n        name: %s,\n        at-entry: %s,\n        at-exit = %s\n      }"
        name x y
    | Registers {at_entry = Some x; at_exit = None} -> 
      Format.sprintf
        "      {\n        name: %s,\n        at-entry: %s\n      }"
        name x
    | Registers {at_entry = None; at_exit = Some y} -> 
      Format.sprintf
        "      {\n        name: %s,\n        at-exit: %s\n      }"
        name y
    | Registers {at_entry = None; at_exit = None} -> 
      Format.sprintf "      {\n        name: %s}" name
    | Memory (Frame (loc, off)) ->
      Format.sprintf
        "      {\n        name: %s,\n        memory: [%s + %s]\n      }"
        name loc (Bap.Std.Word.to_string off)
    | Memory (Global addr) ->
      Format.sprintf
        "      {\n        name: %s,\n        memory: %s\n      }"
        name (Bap.Std.Word.to_string addr)
    | Constant const ->
      Format.sprintf 
        "      {\n        name: %s,\n        constant: %s}"
        name (Bap.Std.Word.to_string const) in
  string_of_value (Hvar.name v) (Hvar.value v)

(* For displaying a patch. *)
let patch_to_string (p : patch) : string =
  let code = match p.patch_code with
  | CCode ccode -> Utils.print_c Cprint.print_def ccode
  | ASMCode asmcode -> asmcode in
  let h_vars =
    String.concat ~sep:"\n" (List.map p.patch_vars ~f:string_of_hvar)
  in
  let extra_constraints = Option.value ~default:"" p.patch_extra_constraints in
  String.concat ~sep:"\n" [
      Printf.sprintf "  {";
      Printf.sprintf "    Patch_name: %s" p.patch_name;
      Printf.sprintf "    Patch_code:\n      %s" code;
      Printf.sprintf "    Patch_point: %s" (Bitvec.to_string p.patch_point);
      Printf.sprintf "    Patch_size: %d" p.patch_size;
      Printf.sprintf "    Patch_vars: [\n%s\n   ]" h_vars;
      Printf.sprintf "    Patch_extra_constaints:\n      %s" extra_constraints;
      Printf.sprintf "  }";
    ]

(* For displaying a list of patches *)
let patches_to_string (ps : patch list) : string =
  String.concat ~sep:",\n" (List.map ~f:patch_to_string ps)

(* For displaying a patch space. *)
let patch_space_to_string (p : patch_space) : string =
  String.concat ~sep:";\n" [
      Printf.sprintf "  {";
      Printf.sprintf "    Space_address: %s" (Int64.to_string p.space_address);
      Printf.sprintf "    Space_size: %s" (Int64.to_string p.space_size);
      Printf.sprintf "  }";
    ]

(* For displaying a list of patch spaces *)
let patch_spaces_to_string (ps : patch_space list) : string =
  String.concat ~sep:",\n" (List.map ~f:patch_space_to_string ps)

(* For displaying WP params. *)
let wp_params_to_string (wp_params : Wp_params.t) : string =
  let opt (s : string option) : string =
    match s with
    | None -> "Nothing"
    | Some s -> s
  in
  let lst (l : string list) : string = String.concat ~sep:", " l in
  let triple (trip : string * string * string) : string =
    match trip with
    | (a, b, c) -> Printf.sprintf "%s; %s; %s" a b c
  in
  let triple_list (t_list : (string * string * string) list) : string =
    lst @@ List.map t_list ~f:triple
  in
  let params = [
      Printf.sprintf "func: %s" wp_params.func;
      Printf.sprintf "precond: %s" wp_params.precond;
      Printf.sprintf "postcond: %s" wp_params.postcond;
      Printf.sprintf "inline: %s" (opt wp_params.inline);
      Printf.sprintf "ext-solver-path: %s" (opt wp_params.ext_solver_path);
      Printf.sprintf "show: %s" (lst wp_params.show);
      Printf.sprintf "use-fun-input-regs: %b" wp_params.use_fun_input_regs;
      Printf.sprintf "fun-specs: %s" (lst wp_params.fun_specs);
      Printf.sprintf
        "user-fun-specs-orig: %s" (triple_list wp_params.user_func_specs_orig);
      Printf.sprintf
        "user-fun-specs-mod: %s" (triple_list wp_params.user_func_specs_mod);
      Printf.sprintf "init-mem: %b" wp_params.init_mem;
    ]
  in
  String.concat ~sep:"\n" params

(* For pretty-printing config. *)
let pp (ppf : Format.formatter) t : unit =
  let info = String.concat ~sep:"\n" [
      Printf.sprintf "Exe: %s" t.exe;
      Printf.sprintf "Patches: %s" (patches_to_string t.patches);
      Printf.sprintf "Output filepath: %s"
        (Option.value t.patched_exe_filepath ~default:"<none provided>");
      Printf.sprintf "Max tries: %d" (Option.value t.max_tries ~default:0);
      Printf.sprintf "Perform verification: %b" t.perform_verification;
      Printf.sprintf "Minizinc model: %s" t.minizinc_model_filepath;
      Printf.sprintf "Ogre file: %s" @@
      Option.value_map t.ogre ~default:"<none provided>" ~f:(function
          | First s -> s
          | Second (None, None) -> "<none provided>"
          | Second (Some orig, None) ->
            Printf.sprintf "orig=%s" orig
          | Second (None, Some mod_) ->
            Printf.sprintf "mod=%s" mod_
          | Second (Some orig, Some mod_) ->
            Printf.sprintf "orig=%s, mod=%s" orig mod_);
      Printf.sprintf "Patch spaces: %s"
        (patch_spaces_to_string t.patch_spaces);
      Printf.sprintf "WP-params: %s" (wp_params_to_string t.wp_params);
    ] in
  Format.fprintf ppf "@[%s@]@." info

(* Create a patch record. *)
let create_patch
    ~patch_name:(patch_name : string)
    ~patch_code:(patch_code : patch_code)
    ~patch_point:(patch_point : Bitvec.t)
    ~patch_size:(patch_size : int)
    ~patch_vars:(patch_vars : Hvar.t list)
    ~patch_sp_align:(patch_sp_align : int)
    ~patch_extra_constraints:(patch_extra_constraints : string option)
  : patch =
  { patch_name; patch_code; patch_point; patch_size; patch_vars;
    patch_sp_align; patch_extra_constraints }

(* Create a configuration record. *)
let create
    ~exe:(exe : string)
    ~patches:(patches : patch list)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    ~max_tries:(max_tries : int option)
    ~perform_verification:(perform_verification : bool)
    ~minizinc_model_filepath:(minizinc_model_filepath : string)
    ~minizinc_isel_filepath:(minizinc_isel_filepath : string option)
    ~ogre:(ogre : ogre_spec option)
    ~patch_spaces:(patch_spaces : patch_space list)
    ~wp_params:(wp_params : Wp_params.t)
  : t =
  { exe; patches; patched_exe_filepath; max_tries; perform_verification;
    minizinc_model_filepath; ogre; patch_spaces; wp_params;
    minizinc_isel_filepath }
