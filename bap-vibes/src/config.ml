(* Implements {!Config}. *)

open !Core_kernel
module Json = Yojson.Safe

(* Extract the patch code, check it's a non-empty string, and parse
into a [Sexp.t list] (it should be a valid S-expression). *)
let patch_code_of_yojson (obj : Json.t)
 : (Sexp.t list, string) Stdlib.result =
  match obj with
  | `String s ->
  begin
    if String.length s = 0 then Result.fail "Missing Patch Code"
    else 
      begin
        try Result.return (Sexp.scan_sexps (Lexing.from_string s))
        with _ -> 
          let msg =
            "Code for patch '" ^ s ^ "' is not a valid S-expression" in
          Result.fail (msg)
      end
  end
  | _ -> Result.fail "Missing Patch Code"

(* Extract the patch point field and parse the hex string into a bitvector, or
   error. *)
let patch_point_of_yojson (obj : Json.t) : (Bitvec.t, string) Stdlib.result =
    match obj with
    | `String s ->
       begin
         try
           Result.return (Bitvec.of_string s)
         with Invalid_argument _ ->
           let msg = Format.sprintf "Invalid hex string: %s" s in
           Result.fail msg
       end
    | _ -> Result.fail "Missing Patch Point"

  (* Extract the property field string and parse it into an S-expression, or
   error. *)
let property_of_yojson (obj : Json.t) : (Sexp.t, string) Stdlib.result =
  match obj with
  | `String s ->
     begin
       try
         Result.return (Sexp.of_string s)
       with Failure _ ->
         let msg = Format.sprintf "Invalid S-expression: %s" s in
         Result.fail msg
     end
  | _ -> Result.fail "Missing Property"

(* A type to represent a patch. *)
type patch =
  {
    (* The name of the patch to use. *)
    patch_name : string [@key "patch-name"];

    (* An s-expression version of the patch's core theory code *)
    patch_code : Sexp.t list [@key "patch-code"] [@of_yojson patch_code_of_yojson];

    (* The address in the original exe to start patching from. *)
    patch_point : Bitvec.t [@key "patch-point"] [@of_yojson patch_point_of_yojson];

    (* The number of bytes of code that the patch replaces or removes,
       beginning at the patch_point *)
    patch_size : int [@key "patch-size"]
  } [@@deriving of_yojson]

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string [@default ""]; (* The filename (path) of the executable to patch. *)
  patches : patch list; (* The list of patches to apply. *)
  func : string; (* The name of the function to check. *)
  property : Sexp.t [@of_yojson property_of_yojson]; (* Correctness property. *)
  patched_exe_filepath : string option [@default None]; (* Optional output location *)
  max_tries : int option 
    [@key "max-tries"] [@default None]; (* Optional number of CEGIS iterations to allow *)
  minizinc_model_filepath : string [@key "minizinc-model"] 
    [@default Constants.default_minizinc_model_filepath]; (* Path to a minizinc model file *)
} [@@deriving of_yojson]

let t_of_yojson ~exe ~patched_exe_filepath (obj : Json.t) : (t, string) result =
  Result.map ~f:(fun t -> {t with exe; patched_exe_filepath}) (of_yojson obj)


(* Patch accessors. *)
let patch_name (p : patch) : string = p.patch_name
let patch_code (p : patch) : Sexp.t list = p.patch_code
let patch_point (p : patch) : Bitvec.t = p.patch_point 
let patch_size (p : patch) : int = p.patch_size

(* Config accessors. *)
let exe t : string = t.exe
let patches t : patch list = t.patches
let func t : string = t.func
let property t : Sexp.t = t.property
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries
let minizinc_model_filepath t : string = t.minizinc_model_filepath

(* For displaying a patch. *)
let patch_to_string (p : patch) : string =
  let code =
    String.concat ~sep:"\n" (List.map p.patch_code ~f:Sexp.to_string) in
  String.concat ~sep:"\n" [
      Printf.sprintf "  {Patch_name: %s" p.patch_name;
      Printf.sprintf "   Patch_code: %s" code;
      Printf.sprintf "   Patch_point: %s" (Bitvec.to_string p.patch_point);
      Printf.sprintf "   Patch_size: %d}" p.patch_size;
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
    ~patch_size:(patch_size : int) : patch =
  { patch_name; patch_code; patch_point; patch_size }

(* Create a configuration record. *)
let create ~exe:(exe : string) ~patches:(patches : patch list)
    ~func:(func : string) ~property:(property : Sexp.t)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    ~max_tries:(max_tries : int option) 
    ~minizinc_model_filepath:(minizinc_model_filepath : string) : t =
  { exe; patches; func; property; patched_exe_filepath;
    max_tries; minizinc_model_filepath }
