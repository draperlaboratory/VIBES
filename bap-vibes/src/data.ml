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

(* This module currently encapsulates our KB ontology.
   We expect much evolution here... *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

module Hvar = Higher_var

module Var_pair = struct
  module T = struct
    type t = var * var [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make(T)
end

type var_pair_set = (Var_pair.t, Var_pair.comparator_witness) Set.t 

let var_pair_set_domain : var_pair_set KB.Domain.t =
  KB.Domain.powerset (module Var_pair)
    ~inspect:Var_pair.sexp_of_t
    "var-pair-set-domain"

let unit_domain : unit KB.Domain.t = KB.Domain.flat
    ~empty:()
    ~equal:Unit.equal
    "unit-domain"

let lab_domain : Theory.label option KB.Domain.t = KB.Domain.optional
    ~equal:Theory.Label.equal
    "label-domain"

(* Optional string domain *)
let string_domain : String.t option KB.Domain.t = KB.Domain.optional
    ~equal:String.(=)
    "string-domain"

(* Optional int domain *)
let int_domain : int option KB.Domain.t = KB.Domain.optional
    ~equal:Int.(=)
    "int-domain"

(* Optional int64 domain *)
let int64_domain : int64 option KB.Domain.t = KB.Domain.optional
    ~equal:Int64.(=)
    "int64-domain"

(* Optional bitvector domain *)
let bitvec_domain : Bitvec.t option KB.Domain.t = KB.Domain.optional
    ~equal:Bitvec.equal
    "bitvec-domain"

(* Optional s-expression domain (e.g., for correctness properties) *)
let sexp_domain : Sexp.t option KB.Domain.t = KB.Domain.optional
    ~equal:Sexp.equal
    "sexp-domain"

(* Optional C definition domain for the patch code. *)
let source_domain : Cabs.definition option KB.Domain.t = KB.Domain.optional
    ~equal:Poly.equal
    "source-domain"

(* Optional string list domain (for assembly). But as per
   Ivan's suggestion, we'll probably make this a powerset domain. *)
let assembly_domain : string list option KB.Domain.t = KB.Domain.optional
    ~equal:(fun x y -> List.equal String.equal x y)
    "assembly-domain"

(* Optional Ir domain for storing ir immediately after translation
   from core_theory *)
let ir_domain : Ir.t option KB.Domain.t = KB.Domain.optional
   ~equal:Ir.equal
   "ir-domain"

(* For storing sets of minizinc solutions *)
let minizinc_solution_domain : Minizinc.sol_set KB.Domain.t =
  KB.Domain.powerset (module Minizinc.Sol) "minizinc-solution-domain"

(* For storing higher variables *)
let higher_vars_domain : Hvar.t list option KB.Domain.t = KB.Domain.optional
  ~equal:(fun x y -> List.equal Hvar.equal x y)
  "higher-vars-domain" 

let exclude_regs_domain : String.Set.t option KB.Domain.t = KB.Domain.optional
  ~equal:String.Set.equal
  "exclude-regs-domain"

let sp_align_domain : int option KB.Domain.t = KB.Domain.optional
  ~equal:Int.equal
  "sp-align-domain"


(* General knowledge info for the package *)
type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value
let package = "vibes"
let name = "data"
let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()

(* Properties pertaining to the patch code *)
module Patch = struct

  (* Declare patches as a KB class.  A patch is a KB object of its own
     corresponding to a single code diff.  A run of the VIBES pipeline may
     involve multiple patches. *)
  type patch_cls
  let patch : (patch_cls, unit) KB.cls =
    KB.Class.declare ~package "patch" ()

  (* This provides equality / comparisons for objects of this class *)
  include (val KB.Object.derive patch)

  let patch_name : (patch_cls, string option) KB.slot =
    KB.Class.property ~package patch "patch-name" string_domain

  let patch_code : (patch_cls, Cabs.definition option) KB.slot =
    KB.Class.property ~package patch "patch-code" source_domain

  let patch_label : (patch_cls, Theory.label option) KB.slot =
    KB.Class.property ~package patch "patch-label" lab_domain

  let raw_ir : (patch_cls, Ir.t option) KB.slot =
    KB.Class.property ~package patch "patch-raw-ir" ir_domain

  let patch_point : (patch_cls, Bitvec.t option) KB.slot =
    KB.Class.property ~package patch "patch-point" bitvec_domain

  let patch_size : (patch_cls, int option) KB.slot =
    KB.Class.property ~package patch "patch-size" int_domain

  let assembly : (patch_cls, string list option) KB.slot =
    KB.Class.property ~package patch "patch-assembly" assembly_domain

  let lang : (patch_cls, Theory.language) KB.slot =
    KB.Class.property ~package patch "patch-language" Theory.Language.domain

  let target : (patch_cls, Theory.target) KB.slot =
    KB.Class.property ~package patch "patch-target" Theory.Target.domain

  let minizinc_solutions : (patch_cls, Minizinc.sol_set) KB.slot =
    KB.Class.property ~package patch "minizinc-solutions"
    minizinc_solution_domain

  let patch_vars : (patch_cls, Hvar.t list option) KB.slot =
    KB.Class.property ~package patch "patch-vars" higher_vars_domain

  let exclude_regs : (patch_cls, String.Set.t option) KB.slot =
    KB.Class.property ~package patch "exclude-regs" exclude_regs_domain

  let sp_align : (patch_cls, int option) KB.slot =
    KB.Class.property ~package patch "sp-align" sp_align_domain

  let congruence : (patch_cls, var_pair_set) KB.slot =
    KB.Class.property ~package patch "congruence" var_pair_set_domain
  
  let set_patch_name (obj : t) (data : string option) : unit KB.t =
    KB.provide patch_name obj data

  let get_patch_name (obj : t) : string option KB.t =
    KB.collect patch_name obj

  let get_patch_name_exn (obj : t) : string KB.t =
    get_patch_name obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_name
    | Some value -> KB.return value

  let set_patch_code (obj : t) (data : Cabs.definition option) : unit KB.t =
    KB.provide patch_code obj data

  let get_patch_code (obj : t) : Cabs.definition option KB.t =
    KB.collect patch_code obj

  let get_patch_code_exn (obj : t) : Cabs.definition KB.t =
    get_patch_code obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_code
    | Some value -> KB.return value

  let set_patch_point (obj : t) (data : Bitvec.t option) : unit KB.t =
    KB.provide patch_point obj data

  let get_patch_point (obj : t) : Bitvec.t option KB.t =
    KB.collect patch_point obj

  let get_patch_point_exn (obj : t) : Bitvec.t KB.t =
    get_patch_point obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_point
    | Some value -> KB.return value

  let set_patch_size (obj : t) (data : int option) : unit KB.t =
    KB.provide patch_size obj data

  let get_patch_size (obj : t) : int option KB.t =
    KB.collect patch_size obj

  let get_patch_size_exn (obj : t) : int KB.t =
    get_patch_size obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_size
    | Some value -> KB.return value

  let init_sem (obj : t) : unit KB.t =
    KB.Object.create Theory.Program.cls >>= fun lab ->
    KB.provide patch_label obj (Some lab)

  let set_bir (obj : t) (sem : Insn.t) : unit KB.t =
    KB.collect patch_label obj >>= fun olab ->
    (* FIXME: fail more gracefully *)
    let lab = Option.value_exn olab in
    KB.provide Theory.Semantics.slot lab sem

  let get_bir (obj : t) : Insn.t KB.t =
    KB.collect patch_label obj >>= fun olab ->
    (* FIXME: fail more gracefully *)
    let lab = Option.value_exn olab in
    KB.collect Theory.Semantics.slot lab

  let set_lang (obj : t) (data : Theory.language) : unit KB.t =
    KB.provide lang obj data

  let get_lang (obj : t) : Theory.language KB.t =
    KB.collect lang obj

  let set_target (obj : t) (data : Theory.target) : unit KB.t =
    KB.provide target obj data

  let get_target (obj : t) : Theory.target KB.t =
    KB.collect target obj

  let set_raw_ir (obj : t) (data : Ir.t option) : unit KB.t =
    KB.provide raw_ir obj data

  let get_raw_ir (obj : t) : Ir.t option KB.t =
    KB.collect raw_ir obj

  let get_raw_ir_exn (obj : t) : Ir.t KB.t =
    get_raw_ir obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_raw_ir
    | Some value -> KB.return value

  let set_exclude_regs (obj : t) (data : String.Set.t option) : unit KB.t =
    KB.provide exclude_regs obj data

  let get_exclude_regs (obj : t) : String.Set.t option KB.t =
    KB.collect exclude_regs obj
  
  let set_assembly (obj : t) (data : string list option) : unit KB.t =
    KB.provide assembly obj data

  let get_assembly (obj : t) : string list option KB.t =
    KB.collect assembly obj

  let get_assembly_exn (obj : t) : string list KB.t =
    get_assembly obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_assembly
    | Some value -> KB.return value

  let get_minizinc_solutions (obj : t) : Minizinc.sol_set KB.t =
    KB.collect minizinc_solutions obj

  let add_minizinc_solution (obj : t) (sol : Minizinc.sol) : unit KB.t =
    KB.provide minizinc_solutions obj (Set.singleton (module Minizinc.Sol) sol)

  let union_minizinc_solution (obj : t) (sol_set : Minizinc.sol_set)
      : unit KB.t =
    KB.provide minizinc_solutions obj sol_set

  let set_patch_vars (obj : t) (data : Hvar.t list option) : unit KB.t =
    KB.provide patch_vars obj data

  let get_patch_vars (obj : t) : Hvar.t list option KB.t =
    KB.collect patch_vars obj

  let get_patch_vars_exn (obj : t) : Hvar.t list KB.t =
    get_patch_vars obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_vars
    | Some value -> KB.return value

  let set_sp_align (obj : t) (data : int option) : unit KB.t =
    KB.provide sp_align obj data

  let get_sp_align (obj : t) : int option KB.t =
    KB.collect sp_align obj

  let get_sp_align_exn (obj : t) : int KB.t =
    get_sp_align obj >>= function
    | None -> Kb_error.fail Kb_error.Missing_sp_align
    | Some value -> KB.return value

  let get_congruence (obj : t) : var_pair_set KB.t =
    KB.collect congruence obj
  
  let set_congruence (obj : t) (data : var * var) : unit KB.t =
    get_congruence obj >>= fun cong ->
    KB.provide congruence obj @@ Set.add cong data
  
end

(* Sets of patches *)
module Patch_set = Set.Make (Patch)

(* Properties pertaining to space for patches *)
module Patch_space = struct

  (* Declare patche spaces as a KB class.  A patch space is a KB object of its
     own corresponding to a single region of space available for patch code.  We
     may know of multiple such regions. *)
  type patch_space_cls
  let patch_space : (patch_space_cls, unit) KB.cls =
    KB.Class.declare ~package "patch-space" ()

  (* This provides equality / comparisons for objects of this class *)
  include (val KB.Object.derive patch_space)

  let offset : (patch_space_cls, int64 option) KB.slot =
    KB.Class.property ~package patch_space "patch-space-offset" int64_domain

  let size : (patch_space_cls, int64 option) KB.slot =
    KB.Class.property ~package patch_space "patch-space-size" int64_domain

  let set_offset (obj : t) (data : int64 option) : unit KB.t =
    KB.provide offset obj data

  let get_offset (obj : t) : int64 option KB.t =
    KB.collect offset obj

  let get_offset_exn (obj : t) : int64 KB.t =
    get_offset obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_space_offset
    | Some value -> KB.return value

  let set_size (obj : t) (data : int64 option) : unit KB.t =
    KB.provide size obj data

  let get_size (obj : t) : int64 option KB.t =
    KB.collect size obj

  let get_size_exn (obj : t) : int64 KB.t =
    get_size obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_space_size
    | Some value -> KB.return value
end

(* Sets of patch spaces *)
module Patch_space_set = Set.Make (Patch_space)

(* Properties pertaining to the original executable *)
module Original_exe = struct

  let patch_spaces_domain : Patch_space_set.t KB.domain =
    KB.Domain.flat ~empty:Patch_space_set.empty ~equal:Patch_space_set.equal
      "patch-spaces"

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "original-exe-filepath"
      string_domain

  let target : (cls, Theory.target) KB.slot =
    KB.Class.property ~package cls "original-exe-target"
      Theory.Target.domain

  let patch_spaces : (cls, Patch_space_set.t) KB.slot =
    KB.Class.property ~package cls "original-exe-patch-spaces"
      patch_spaces_domain

  let set_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide filepath obj data

  let get_filepath (obj : t) : string option KB.t =
    KB.collect filepath obj

  let get_filepath_exn (obj : t) : string KB.t =
    get_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_original_exe_filepath
    | Some value -> KB.return value

  let set_target (obj : t) (data : Theory.target) : unit KB.t =
    KB.provide target obj data

  let get_target (obj : t) : Theory.target KB.t =
    KB.collect target obj

  let get_target_exn (obj : t) : Theory.target KB.t =
    get_target obj >>= fun tgt ->
    if KB.Domain.is_empty Theory.Target.domain tgt then
      Kb_error.fail Kb_error.Missing_target
    else
      KB.return tgt

  let set_patch_spaces (obj : t) (data : Patch_space_set.t) : unit KB.t =
    KB.provide patch_spaces obj data

  let get_patch_spaces (obj : t) : Patch_space_set.t KB.t =
    KB.collect patch_spaces obj

end

(* Properties pertaining to the patched executable *)
module Patched_exe = struct

  let patch_domain : Patch_set.t KB.domain =
    KB.Domain.flat ~empty:Patch_set.empty ~equal:Patch_set.equal "patches"

  let patches : (cls, Patch_set.t) KB.slot =
    KB.Class.property ~package cls "patches" patch_domain

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "patched-exe-filepath"
      string_domain

  let tmp_filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "tmp-patched-exe-filepath"
      string_domain

  let set_patches (obj : t) (data : Patch_set.t) : unit KB.t =
    KB.provide patches obj data

  let get_patches (obj : t) : (Patch_set.t) KB.t =
    KB.collect patches obj

  let set_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide filepath obj data

  let get_filepath (obj : t) : string option KB.t =
    KB.collect filepath obj

  let get_filepath_exn (obj : t) : string KB.t =
    get_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patched_exe_filepath
    | Some value -> KB.return value

  let set_tmp_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide tmp_filepath obj data

  let get_tmp_filepath (obj : t) : string option KB.t =
    KB.collect tmp_filepath obj

  let get_tmp_filepath_exn (obj : t) : string KB.t =
    get_tmp_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_tmp_patched_exe_filepath
    | Some value -> KB.return value

end

(* Properties pertaining to the solver *)
module Solver = struct

  let minizinc_model_filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "minizinc-model-filepath" string_domain

  let set_minizinc_model_filepath (obj : t) (data : string option)
      : unit KB.t =
    KB.provide minizinc_model_filepath obj data

  let get_minizinc_model_filepath (obj : t) : string option KB.t =
    KB.collect minizinc_model_filepath obj

  let get_minizinc_model_filepath_exn (obj : t) : string KB.t =
    get_minizinc_model_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_minizinc_model_filepath
    | Some value -> KB.return value

end
