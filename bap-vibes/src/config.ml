(* Implements {!Config}. *)

open !Core_kernel

module Hvar = Higher_var

(* A type to represent a patch. *)
type patch =
  {
    (* The name of the patch to use. *)
    patch_name : string;

    (* An s-expression version of the patch's core theory code *)
    patch_code : Sexp.t list;

    (* The address in the original exe to start patching from. *)
    patch_point : Bitvec.t;

    (* The number of bytes of code that the patch replaces or removes,
       beginning at the patch_point *)
    patch_size : int;

    (* Higher variables *)
    patch_vars : Hvar.t list
  }

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string; (* The filename (path) of the executable to patch. *)
  patches : patch list; (* The list of patches to apply. *)
  func : string; (* The name of the function to check. *)
  property : Sexp.t; (* Correctness property. *)
  patched_exe_filepath : string option; (* Optional output location *)
  max_tries : int option; (* Optional number of CEGIS iterations to allow *)
  minizinc_model_filepath : string; (* Path to a minizinc model file *)
}

(* Patch accessors. *)
let patch_name (p : patch) : string = p.patch_name
let patch_code (p : patch) : Sexp.t list = p.patch_code
let patch_point (p : patch) : Bitvec.t = p.patch_point 
let patch_size (p : patch) : int = p.patch_size
let patch_vars (p : patch) : Hvar.t list = p.patch_vars

(* Config accessors. *)
let exe t : string = t.exe
let patches t : patch list = t.patches
let func t : string = t.func
let property t : Sexp.t = t.property
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries
let minizinc_model_filepath t : string = t.minizinc_model_filepath

(* For displaying a higher var. *)
let string_of_hvar (v : Hvar.t) : string =
  let string_of_loc (v_loc : Hvar.stored_in) : string =
    match v_loc with
    | Hvar.Register reg -> reg
    | Hvar.Memory (fp, offset) ->
      Format.sprintf "%s - %s" fp (Bitvec.to_string offset)
  in
  let part_1 = Format.sprintf
    "      {\n        name: %s,\n        at-entry: %s\n"
    (Hvar.name v) (string_of_loc (Hvar.at_entry v))
  in
  let part_2 = Format.sprintf
    "        at-exit: %s\n      }"
    (string_of_loc (Hvar.at_exit v))
  in
  String.concat ~sep:"" [part_1; part_2]

(* For displaying a patch. *)
let patch_to_string (p : patch) : string =
  let code =
    String.concat ~sep:"\n      " (List.map p.patch_code ~f:Sexp.to_string) in
  let h_vars =
    String.concat ~sep:"\n" (List.map p.patch_vars ~f:string_of_hvar) in
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

(* For pretty-printing config. *)
let pp (ppf : Format.formatter) t : unit =
  let info = String.concat ~sep:"\n" [
      Printf.sprintf "Exe: %s" t.exe;
      Printf.sprintf "Patches: %s" (patches_to_string t.patches);
      Printf.sprintf "Func: %s" t.func;
      Printf.sprintf "Property: %s" (Sexp.to_string t.property);
      Printf.sprintf "Output filepath: %s"
        (Option.value t.patched_exe_filepath ~default:"none provided");
      Printf.sprintf "Max tries: %d" (Option.value t.max_tries ~default:0);
      Printf.sprintf "Minizinc model: %s" t.minizinc_model_filepath;
    ] in
  Format.fprintf ppf "@[%s@]@." info

(* Create a patch record. *)
let create_patch ~patch_name:(patch_name : string)
    ~patch_code:(patch_code : Sexp.t list)
    ~patch_point:(patch_point : Bitvec.t)
    ~patch_size:(patch_size : int) 
    ~patch_vars:(patch_vars : Hvar.t list) : patch =
  { patch_name; patch_code; patch_point; patch_size; patch_vars; }

(* Create a configuration record. *)
let create ~exe:(exe : string) ~patches:(patches : patch list)
    ~func:(func : string) ~property:(property : Sexp.t)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    ~max_tries:(max_tries : int option) 
    ~minizinc_model_filepath:(minizinc_model_filepath : string) : t =
  { exe; patches; func; property; patched_exe_filepath;
    max_tries; minizinc_model_filepath }
