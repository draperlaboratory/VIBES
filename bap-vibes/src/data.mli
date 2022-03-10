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

(** This module captures our KB "ontology" - the collection of classes and
    properties that the VIBES toolchain defines and manipulates *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

module Hvar = Higher_var

module Var_pair : sig
  type t = var * var [@@deriving compare, sexp]
  include Comparator.S with type t := t
end

type var_pair_set = (Var_pair.t, Var_pair.comparator_witness) Set.t

type ins_outs = {ins : Var.Set.t; outs: Var.Set.t} [@@deriving compare, equal, sexp]

type sol = {
  reg : var Var.Map.t;
  opcode : Ir.opcode Int.Map.t;
  temp : Var.t Var.Map.t;
  active : bool Int.Map.t;
  issue : int Int.Map.t;
} [@@deriving sexp, compare]

(** This is a module necessary for building Sets of [sol] *)
module Sol : sig
  module S :
  sig
    type t = sol
    val compare : sol -> sol -> int
    val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
  end
  type t = sol
  val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
  val compare : S.t -> S.t -> int
  type comparator_witness = Base__Comparable.Make(S).comparator_witness
  val comparator : (S.t, comparator_witness) Base__.Comparator.comparator
end

type sol_set = (sol, Sol.comparator_witness) Core_kernel.Set.t

(** We define "domains" for the types used in our properties. *)
val string_domain       : string option KB.Domain.t
val int_domain          : int option KB.Domain.t
val int64_domain        : int64 option KB.Domain.t
val bitvec_domain       : Bitvec.t option KB.Domain.t
val sexp_domain         : Sexp.t option KB.Domain.t
val source_domain       : Cabs.definition option KB.Domain.t
val assembly_domain     : string list option KB.Domain.t
val unit_domain         : unit KB.Domain.t
val higher_vars_domain  : Hvar.t list option KB.Domain.t
val var_pair_set_domain : var_pair_set KB.Domain.t
val ins_outs_map_domain : ins_outs Tid.Map.t KB.Domain.t

(** These are the top-level class definitions.

    - type [cls] is the class of the top-level VIBES KB object that stores
      all data related to a run in properties.
    - type [t] is the type of objects of that class.
    - val [cls] is the declared class.
    - type [computed] is the type of the result computed by [KB.run]. *)
type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value

val package : string
val name : string
val cls : (cls, unit) KB.cls

(** The patch module defines an additional class holding all properties
    related to a specific patch fragment - a contiguous region of code that
    is being patched into the binary. The outer class defined above holds a
    collection of these (see {!Patched_exe.patches}). *)
module Patch : sig

  (* The KB infrastructure *)
  type patch_cls
  type t = patch_cls KB.obj
  val patch : (patch_cls, unit) KB.cls

  (* equal, compare, and other things for patches *)
  include Knowledge.Object.S with type t := t

  val patch_name : (patch_cls, string option) KB.slot
  val patch_code : (patch_cls, Cabs.definition option) KB.slot
  val patch_point : (patch_cls, Bitvec.t option) KB.slot
  val patch_size : (patch_cls, int option) KB.slot
  val patch_label : (patch_cls, Theory.label option) KB.slot
  val raw_ir : (patch_cls, (Ir.t * Graphs.Tid.t) option) KB.slot
  val exclude_regs : (patch_cls, String.Set.t option) KB.slot
  val congruence : (patch_cls, var_pair_set) KB.slot
  val ins_outs_map : (patch_cls, ins_outs Tid.Map.t) KB.slot
  val assembly : (patch_cls, string list option) KB.slot
  val sp_align : (patch_cls, int option) KB.slot
  (* The language/encoding of the assembly, typically used to
     distinguish between ARM and Thumb. *)
  (* TODO: add the target as well. *)
  val lang : (patch_cls, Theory.language) KB.slot
  val target : (patch_cls, Theory.target) KB.slot
  val minizinc_solutions : (patch_cls, sol_set) KB.slot
  (* High variables for the patch *)
  val patch_vars : (patch_cls, Hvar.t list option) KB.slot

  val set_patch_name : t -> string option -> unit KB.t
  val get_patch_name : t -> string option KB.t
  val get_patch_name_exn : t -> string KB.t

  val set_patch_code : t -> Cabs.definition option -> unit KB.t
  val get_patch_code : t -> Cabs.definition option KB.t
  val get_patch_code_exn : t -> Cabs.definition KB.t

  val set_patch_point : t -> Bitvec.t option -> unit KB.t
  val get_patch_point : t -> Bitvec.t option KB.t
  val get_patch_point_exn : t -> Bitvec.t KB.t

  val set_patch_size : t -> int option -> unit KB.t
  val get_patch_size : t -> int option KB.t
  val get_patch_size_exn : t -> int KB.t

  (* This initializes the semantics slot by creating a program label
     that will contain the semantics. This *must* be called before
     set_bir! *)
  val init_sem : t -> unit KB.t
  val set_bir : t -> insn -> unit KB.t
  val get_bir : t -> insn KB.t

  val set_raw_ir : t -> (Ir.t * Graphs.Tid.t) option -> unit KB.t
  val get_raw_ir : t -> (Ir.t * Graphs.Tid.t) option KB.t
  val get_raw_ir_exn : t -> (Ir.t * Graphs.Tid.t) KB.t

  val set_exclude_regs : t -> String.Set.t option -> unit KB.t
  val get_exclude_regs : t -> String.Set.t option KB.t
  
  val set_assembly : t -> string list option -> unit KB.t
  val get_assembly : t -> string list option KB.t
  val get_assembly_exn : t -> string list KB.t

  val set_lang : t -> Theory.language -> unit KB.t
  val get_lang : t -> Theory.language KB.t

  val set_target : t -> Theory.target -> unit KB.t
  val get_target : t -> Theory.target KB.t

  val get_minizinc_solutions : t -> sol_set KB.t
  val add_minizinc_solution : t -> sol -> unit KB.t
  val union_minizinc_solution : t -> sol_set -> unit KB.t

  val set_patch_vars : t -> Hvar.t list option -> unit KB.t
  val get_patch_vars : t -> Hvar.t list option KB.t
  val get_patch_vars_exn : t -> Hvar.t list KB.t

  val set_sp_align : t -> int option -> unit KB.t
  val get_sp_align : t -> int option KB.t
  val get_sp_align_exn : t -> int KB.t

  val set_congruence : t -> var_pair_set -> unit KB.t
  val add_congruence : t -> var * var -> unit KB.t
  val get_congruence : t -> var_pair_set KB.t

  val set_ins_outs_map : t -> ins_outs Tid.Map.t -> unit KB.t
  val get_ins_outs_map : t -> ins_outs Tid.Map.t KB.t

  val set_extra_constraints : t -> string option -> unit KB.t
  val get_extra_constraints : t -> string option KB.t
end

(** Sets of patches *)
module Patch_set : Set.S with type Elt.t = Patch.t

(** The Patch_space module defines an additional class holding all properties
   related to regions in the original binary that can be used for the patch
   (e.g., dead code).  *)
module Patch_space : sig

  (* The KB infrastructure *)
  type patch_space_cls
  type t = patch_space_cls KB.obj
  val patch_space : (patch_space_cls, unit) KB.cls

  (* equal, compare, and other things for patch spaces *)
  include Knowledge.Object.S with type t := t

  val offset : (patch_space_cls, int64 option) KB.slot
  val size : (patch_space_cls, int64 option) KB.slot

  val set_offset : t -> int64 option -> unit KB.t
  val get_offset : t -> int64 option KB.t
  val get_offset_exn : t -> int64 KB.t

  val set_size : t -> int64 option -> unit KB.t
  val get_size : t -> int64 option KB.t
  val get_size_exn : t -> int64 KB.t
end

(** Sets of patch spaces *)

module Patch_space_set : Set.S with type Elt.t = Patch_space.t

(** Properties pertaining to the original executable *)
module Original_exe : sig
  val filepath : (cls, string option) KB.slot
  val target : (cls, Theory.target) KB.slot
  val patch_spaces : (cls, Patch_space_set.t) KB.slot

  val set_filepath : t -> string option -> unit KB.t
  val get_filepath : t -> string option KB.t
  val get_filepath_exn : t -> string KB.t

  val set_target : t -> Theory.target -> unit KB.t
  val get_target : t -> Theory.target KB.t

  (** Raises if the target is empty *)
  val get_target_exn : t -> Theory.target KB.t

  val set_patch_spaces : t -> Patch_space_set.t -> unit KB.t
  val get_patch_spaces : t -> Patch_space_set.t KB.t

end

(** Properties pertaining to the patched executable *)
module Patched_exe : sig

  val patches : (cls, Patch_set.t) KB.slot
  val filepath : (cls, string option) KB.slot
  val tmp_filepath : (cls, string option) KB.slot

  val set_patches : t -> Patch_set.t -> unit KB.t
  val get_patches : t -> Patch_set.t KB.t

  val set_filepath : t -> string option -> unit KB.t
  val get_filepath : t -> string option KB.t
  val get_filepath_exn : t -> string KB.t

  val set_tmp_filepath : t -> string option -> unit KB.t
  val get_tmp_filepath : t -> string option KB.t
  val get_tmp_filepath_exn : t -> string KB.t
end

(** Properties pertaining to the solver *)
module Solver : sig
  val minizinc_model_filepath : (cls, string option) KB.slot
  val set_minizinc_model_filepath : t -> string option -> unit KB.t
  val get_minizinc_model_filepath : t -> string option KB.t
  val get_minizinc_model_filepath_exn : t -> string KB.t
end
