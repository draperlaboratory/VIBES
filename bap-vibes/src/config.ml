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
  }

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string; (* The filename (path) of the executable to patch. *)
  patches : patch list; (* The list of patches to apply. *)
  patched_exe_filepath : string option; (* Optional output location *)
  max_tries : int option; (* Optional number of CEGIS iterations to allow *)
  minizinc_model_filepath : string; (* Path to a minizinc model file *)
  ogre : string option;
  wp_params : Wp_params.t;
}

(* Patch accessors. *)
let patch_name (p : patch) : string = p.patch_name
let patch_code (p : patch) : patch_code = p.patch_code
let patch_point (p : patch) : Bitvec.t = p.patch_point
let patch_size (p : patch) : int = p.patch_size
let patch_vars (p : patch) : Hvar.t list = p.patch_vars
let patch_sp_align (p : patch) : int = p.patch_sp_align

(* Config accessors. *)
let exe t : string = t.exe
let patches t : patch list = t.patches
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries
let minizinc_model_filepath t : string = t.minizinc_model_filepath
let ogre t : string option = t.ogre
let wp_params t : Wp_params.t = t.wp_params

(* For displaying a higher var. *)
let string_of_hvar (v : Hvar.t) : string =
  let string_of_loc (v_loc : Hvar.stored_in) : string =
    match Hvar.register v_loc with
    | Some reg -> reg
    | None -> match Hvar.memory v_loc with
      | None -> failwith "Higher var with storage must be either a \
                          register or memory!"
      | Some memory -> match Hvar.frame memory with
        | Some (fp, offset) ->
          Format.sprintf "[%s + %s]" fp (Bap.Std.Word.to_string offset)
        | None -> match Hvar.global memory with
          | None -> failwith "Higher var with memory storage must be \
                              either a frame or a global!"
          | Some addr -> Format.sprintf "[%s]" (Bap.Std.Word.to_string addr)
  in
  let string_of_value (name : string) (v : Hvar.value) : string =
    match Hvar.at_entry v with
    | Some v_loc ->
      let part_1 =
        Format.sprintf
          "      {\n        name: %s,\n        at-entry: %s\n"
          name (string_of_loc v_loc) in
      begin
        match Hvar.at_exit v with
        | None -> part_1 ^ "\n      }"
        | Some v_loc_2 ->
          let part_2 =
            Format.sprintf
              "        at-exit: %s\n      }"
              (string_of_loc v_loc_2) in
          String.concat ~sep:"" [part_1; part_2]
      end
    | None -> match Hvar.constant v with
      | None -> failwith "Higher var must be either a constant or have \
                          a storage classification!"
      | Some const ->
        Format.sprintf 
          "      {\n        name: %s,\n        constant: %s}"
          name (Bap.Std.Word.to_string const)
  in
  string_of_value (Hvar.name v) (Hvar.value v)

(* For displaying a patch. *)
let patch_to_string (p : patch) : string =
  let code = match p.patch_code with
  | CCode ccode -> Utils.print_c Cprint.print_def ccode
  | ASMCode asmcode -> asmcode in
  let h_vars =
    String.concat ~sep:"\n" (List.map p.patch_vars ~f:string_of_hvar)
  in
  String.concat ~sep:"\n" [
      Printf.sprintf "  {";
      Printf.sprintf "    Patch_name: %s" p.patch_name;
      Printf.sprintf "    Patch_code:\n      %s" code;
      Printf.sprintf "    Patch_point: %s" (Bitvec.to_string p.patch_point);
      Printf.sprintf "    Patch_size: %d" p.patch_size;
      Printf.sprintf "    Patch_vars: [\n%s\n   ]" h_vars;
      Printf.sprintf "  }";
    ]

(* For displaying a list of patches *)
let patches_to_string (ps : patch list) : string =
  String.concat ~sep:",\n" (List.map ~f:patch_to_string ps)

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
    ] 
  in
  String.concat ~sep:"\n" params

(* For pretty-printing config. *)
let pp (ppf : Format.formatter) t : unit =
  let info = String.concat ~sep:"\n" [
      Printf.sprintf "Exe: %s" t.exe;
      Printf.sprintf "Patches: %s" (patches_to_string t.patches);
      Printf.sprintf "Output filepath: %s"
        (Option.value t.patched_exe_filepath ~default:"none provided");
      Printf.sprintf "Max tries: %d" (Option.value t.max_tries ~default:0);
      Printf.sprintf "Minizinc model: %s" t.minizinc_model_filepath;
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
  : patch =
  { patch_name; patch_code; patch_point; patch_size; patch_vars; patch_sp_align }

(* Create a configuration record. *)
let create
    ~exe:(exe : string)
    ~patches:(patches : patch list)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    ~max_tries:(max_tries : int option)
    ~minizinc_model_filepath:(minizinc_model_filepath : string)
    ~ogre:(ogre : string option)
    ~wp_params:(wp_params : Wp_params.t)
  : t =
  { exe; patches; patched_exe_filepath;
    max_tries; minizinc_model_filepath; ogre; wp_params }
