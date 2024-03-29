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

open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Files = Vibes_utils.Files
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces
module JSON = Vibes_utils.Json

let (let*) x f = Result.bind x ~f

type patch = {
  name : string;
  patch : string;
  patch_info : string;
  bir : string;
  bir_opt : string;
  vir : string;
  asm : string;
  constraints : string;
}

type t = {
  target : T.target;
  language : T.language;
  model : string;
  binary : string;
  patched_binary : string;
  patches : patch list;
  spaces : string;
  ogre : string option;
}

let create_patch (patch : string) : patch = {
  name = patch;
  patch = sprintf "%s.c" patch;
  patch_info = sprintf "%s.info.json" patch;
  bir = sprintf "%s.bir" patch;
  bir_opt = sprintf "%s.opt.bir" patch;
  vir = sprintf "%s.vir" patch;
  asm = sprintf "%s.asm" patch;
  constraints = sprintf "%s.mzn" patch;
}

let target_of_spec (spec : Ogre.doc) : T.target KB.t =
  let open KB.Syntax in
  KB.Object.scoped T.Unit.cls @@ fun unit ->
  let* () = KB.provide Image.Spec.slot unit spec in
  KB.collect T.Unit.target unit

let entry_of_spec (spec : Ogre.doc) : int64 KB.t =
  match Ogre.(eval (require Image.Scheme.entry_point) spec) with
  | Error e -> KB.fail @@ Errors.No_entry_point "No entry point found"
  | Ok entry -> KB.return entry

let unsupported_target (target : T.target) : _ KB.t =
  let msg = Format.asprintf "Unsupported target %a" T.Target.pp target in
  KB.fail @@ Errors.Unsupported_target msg

let language_of_spec (spec : Ogre.doc) (target : T.target) : T.language KB.t =
  let open KB.Syntax in
  if CT.is_arm32 target then
    let+ entry = entry_of_spec spec in
    if Int64.((entry land 1L) <> 0L)
    then Arm_target.llvm_t32
    else Arm_target.llvm_a32
  else if CT.is_ppc32 target then !!Bap_powerpc_target.llvm_powerpc32
  else unsupported_target target

let language_matches_target
    (target : T.target)
    (language : T.language) : unit KB.t =
  let msg = Format.asprintf "Invalid target/language combination (%a/%a)"
      T.Target.pp target T.Language.pp language in
  let eq = T.Language.equal language in
  if CT.is_arm32 target then
    if eq Arm_target.llvm_t32
    || eq Arm_target.llvm_a32 then KB.return ()
    else KB.fail @@ Errors.Invalid_target_lang msg
  else if CT.is_ppc32 target then
    if eq Bap_powerpc_target.llvm_powerpc32 then KB.return ()
    else KB.fail @@ Errors.Invalid_target_lang msg
  else unsupported_target target

let target_and_lang = Toplevel.var "target-and-lang"

let infer_target_and_lang
    ?(language : T.language option = None)
    (binary : string) : (T.target * T.language, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  let* image = Vibes_utils.Loader.image binary in
  let spec = Image.spec image in try
    Toplevel.put target_and_lang begin
      let open KB.Syntax in
      let* target = target_of_spec spec in
      let+ language = match language with
        | None -> language_of_spec spec target
        | Some language ->
          let+ () = language_matches_target target language in
          language in
      target, language
    end;
    Ok (Toplevel.get target_and_lang)
  with Toplevel.Conflict c -> Error c

let create
    ?(ogre : string option = None)
    ?(language : T.language option = None)
    ~(patch_names : string list)
    ~(model : string)
    ~(binary : string)
    ~(patched_binary : string)
    ~(spaces : string)
    () : (t, KB.conflict) result =
  let (let+) x f = Result.map x ~f in
  let+ target, language = infer_target_and_lang binary ~language in {
    target;
    language;
    model;
    binary;
    patched_binary;
    spaces;
    patches = List.map patch_names ~f:create_patch;
    ogre;
  }

let pp_makefile (ppf : Format.formatter) (t : t) : unit =
  (* Toplevel comment. *)
  Format.fprintf ppf "# This file was generated by vibes-init\n%!";
  Format.fprintf ppf "# Please edit at your own risk\n\n%!";
  (* Base definitions. *)
  Format.fprintf ppf "# Base definitions\n\n%!";
  Format.fprintf ppf "TARGET := %a\n%!" T.Target.pp t.target;
  Format.fprintf ppf "LANG := %a\n%!" T.Language.pp t.language;
  Format.fprintf ppf "MODEL := %s\n%!" t.model;
  Format.fprintf ppf "BINARY := %s\n%!" t.binary;
  Format.fprintf ppf "PATCHED_BINARY := %s\n%!" t.patched_binary;
  Option.iter t.ogre ~f:(Format.fprintf ppf "OGRE := %s\n%!");
  Format.fprintf ppf "SPACES := %s\n\n%!" t.spaces;
  (* Patch definitions. *)
  List.iteri t.patches ~f:(fun i p ->
      Format.fprintf ppf "# Definitions for patch %s (%d)\n\n%!" p.name i;
      Format.fprintf ppf "PATCH_%d := %s\n%!" i p.patch;
      Format.fprintf ppf "PATCH_%d_INFO := %s\n%!" i p.patch_info;
      Format.fprintf ppf "PATCH_%d_BIR := %s\n%!" i p.bir;
      Format.fprintf ppf "PATCH_%d_BIR_OPT := %s\n%!" i p.bir_opt;
      Format.fprintf ppf "PATCH_%d_VIR := %s\n%!" i p.vir;
      Format.fprintf ppf "PATCH_%d_ASM := %s\n%!" i p.asm;
      Format.fprintf ppf "PATCH_%d_CONSTRAINTS := %s\n\n%!" i p.constraints);
  (* Patch targets. *)
  List.iteri t.patches ~f:(fun i p ->
      Format.fprintf ppf "# Targets for patch %s (%d)\n\n%!" p.name i;
      (* parse *)
      Format.fprintf ppf ".PHONY: parse%d\n%!" i;
      Format.fprintf ppf "parse%d:\n%!" i;
      Format.fprintf ppf "\trm -f $(PATCH_%d_BIR)\n%!" i;
      Format.fprintf ppf "\t$(MAKE) $(PATCH_%d_BIR)\n\n%!" i;
      Format.fprintf ppf "$(PATCH_%d_BIR): \
                          $(PATCH_%d) $(PATCH_%d_INFO)\n%!" i i i;
      Format.fprintf ppf "\tvibes-parse \
                          --target $(TARGET) \
                          --patch-filepath $(PATCH_%d) \
                          --patch-info-filepath $(PATCH_%d_INFO) \
                          --bir-outfile $(PATCH_%d_BIR) \
                          --verbose\n\n%!" i i i;
      (* opt *)
      Format.fprintf ppf ".PHONY: opt%d\n%!" i;
      Format.fprintf ppf "opt%d:\n%!" i;
      Format.fprintf ppf "\trm -f $(PATCH_%d_BIR_OPT)\n%!" i;
      Format.fprintf ppf "\t$(MAKE) $(PATCH_%d_BIR_OPT)\n\n%!" i;
      Format.fprintf ppf "$(PATCH_%d_BIR_OPT): $(PATCH_%d_BIR)\n%!" i i;
      Format.fprintf ppf "\tvibes-opt \
                          --target $(TARGET) \
                          --language $(LANG) \
                          --patch-info-filepath $(PATCH_%d_INFO) \
                          --bir-filepath $(PATCH_%d_BIR) \
                          --bir-outfile $(PATCH_%d_BIR_OPT) \
                          --verbose\n\n%!" i i i;
      (* select *)      
      Format.fprintf ppf ".PHONY: select%d\n%!" i;
      Format.fprintf ppf "select%d:\n%!" i;
      Format.fprintf ppf "\trm -f $(PATCH_%d_VIR)\n%!" i;
      Format.fprintf ppf "\t$(MAKE) $(PATCH_%d_VIR)\n\n%!" i;
      Format.fprintf ppf "$(PATCH_%d_VIR): $(PATCH_%d_BIR_OPT)\n%!" i i;
      Format.fprintf ppf "\tvibes-select \
                          --target $(TARGET) \
                          --language $(LANG) \
                          --patch-info-filepath $(PATCH_%d_INFO) \
                          --bir-filepath $(PATCH_%d_BIR_OPT) \
                          --vir-outfile $(PATCH_%d_VIR) \
                          --verbose\n\n%!" i i i;
      (* as *)
      Format.fprintf ppf ".PHONY: as%d\n%!" i;
      Format.fprintf ppf "as%d:\n%!" i;
      Format.fprintf ppf "\trm -f $(PATCH_%d_ASM)\n%!" i;
      Format.fprintf ppf "\t$(MAKE) $(PATCH_%d_ASM)\n\n%!" i;
      Format.fprintf ppf "$(PATCH_%d_ASM): $(PATCH_%d_VIR)\n%!" i i;
      Format.fprintf ppf "\tvibes-as \
                          --target $(TARGET) \
                          --language $(LANG) \
                          --vir-filepath $(PATCH_%d_VIR) \
                          --asm-outfile $(PATCH_%d_ASM) \
                          --model $(MODEL) \
                          --patch-info-filepath $(PATCH_%d_INFO) \
                          --extra-constraints $(PATCH_%d_CONSTRAINTS) \
                          --verbose\n\n%!" i i i i;
      (* clean *)
      Format.fprintf ppf ".PHONY: clean%d\n%!" i;
      Format.fprintf ppf "clean%d:\n%!" i;
      Format.fprintf ppf "\trm -f \
                          $(PATCH_%d_BIR) \
                          $(PATCH_%d_BIR_OPT) \
                          $(PATCH_%d_VIR) \
                          $(PATCH_%d_ASM) \
                          $(PATCHED_BINARY)\n\n%!" i i i i);
  Format.fprintf ppf "# Main targets\n\n%!";
  (* parse *)
  let parses =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "parse%d") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: parse\n%!";
  Format.fprintf ppf "parse: %s\n\n%!" parses;
  (* opt *)
  let opts =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "opt%d") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: opt\n%!";
  Format.fprintf ppf "opt: %s\n\n%!" opts;
  (* select *)
  let selects =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "select%d") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: select\n%!";
  Format.fprintf ppf "select: %s\n\n%!" selects;
  (* as *)
  let ases =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "as%d") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: as\n%!";
  Format.fprintf ppf "as: %s\n\n%!" ases;
  (* patch *)
  let asms =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "$(PATCH_%d_ASM)") in
  let asms_target = String.concat asms ~sep:" " in
  let asms_arg = String.concat asms ~sep:"," in
  Format.fprintf ppf ".PHONY: patch\n%!";
  Format.fprintf ppf "patch:\n";
  Format.fprintf ppf "\trm -f $(PATCHED_BINARY)\n%!";
  Format.fprintf ppf "\t$(MAKE) $(PATCHED_BINARY)\n\n%!";
  begin match t.ogre with
    | None ->
      Format.fprintf ppf "$(PATCHED_BINARY): %s\n%!" asms_target;
      Format.fprintf ppf "\tvibes-patch \
                          --target $(TARGET) \
                          --language $(LANG) \
                          --binary $(BINARY) \
                          --patch-spaces $(SPACES) \
                          --asm-filepaths %s \
                          --patched-binary $(PATCHED_BINARY) \
                          --verbose\n%!" asms_arg
    | Some _ ->
      Format.fprintf ppf "$(PATCHED_BINARY): %s $(OGRE)\n%!" asms_target;
      Format.fprintf ppf "\tvibes-patch \
                          --target $(TARGET) \
                          --language $(LANG) \
                          --binary $(BINARY) \
                          --patch-spaces $(SPACES) \
                          --asm-filepaths %s \
                          --patched-binary $(PATCHED_BINARY) \
                          --ogre $(OGRE) \
                          --verbose\n%!" asms_arg
  end;
  Format.fprintf ppf "\tchmod +x $(PATCHED_BINARY)\n\n%!";
  (* clean *)
  let cleans =
    List.mapi t.patches ~f:const |>
    List.map ~f:(sprintf "clean%d") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: clean\n%!";
  Format.fprintf ppf "clean: %s\n\n%!" cleans;
  (* all *)
  Format.fprintf ppf ".DEFAULT_GOAL := all\n%!";
  Format.fprintf ppf "all: $(PATCHED_BINARY)\n%!"

let dummy_patch_info (target : T.target) : Patch_info.t = {
  patch_point =
    Addr.of_string @@
    Format.sprintf "0x0:%d" @@
    T.Target.code_addr_size target;
  patch_size = 0L;
  sp_align = 0;
  patch_vars = [];
}

let write (data : string) (file : string) : (unit, KB.conflict) result =
  if Caml.Sys.file_exists file then Ok ()
  else Files.write_or_error data file

let generate_patch_files
    (target : T.target)
    (i : int)
    (p : patch) : (unit, KB.conflict) result =
  let* () = write "" p.patch in
  let* () = write "" p.constraints in
  let info_data =
    Format.asprintf "%a" Patch_info.pp @@
    dummy_patch_info target in
  let* () = write info_data p.patch_info in
  Ok ()

let generate_files (t : t) : (unit, KB.conflict) result =
  let spaces_data = Format.asprintf "%a" Spaces.pp Spaces.empty in
  let* () = write spaces_data t.spaces in
  let rec patch i = function
    | [] -> Ok ()
    | p :: rest ->
      let* () = generate_patch_files t.target i p in
      patch (i + 1) rest in
  let* () = patch 0 t.patches in
  Ok ()
