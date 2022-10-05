open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Files = Vibes_utils.Files
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces
module JSON = Vibes_utils.Json

let (let*) x f = Result.bind x ~f

type patch = {
  name : string;
  patch : string;
  patch_info : string;
  func_info : string;
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
  ogre : string;
  patches : patch list;
  spaces : string;
}

let create_patch (patch : string) : patch = {
  name = patch;
  patch = sprintf "%s.c" patch;
  patch_info = sprintf "%s.info.json" patch;
  func_info = sprintf "%s.func.json" patch;
  bir = sprintf "%s.bir" patch;
  bir_opt = sprintf "%s.opt.bir" patch;
  vir = sprintf "%s.vir" patch;
  asm = sprintf "%s.asm" patch;
  constraints = sprintf "%s.mzn" patch;
}

let create
    (target : T.target)
    (language : T.language)
    ~(patch_names : string list)
    ~(model : string)
    ~(binary : string)
    ~(patched_binary : string)
    ~(ogre : string)
    ~(spaces : string) : t = {
  target;
  language;
  model;
  binary;
  patched_binary;
  ogre;
  spaces;
  patches = List.map patch_names ~f:create_patch;
}

let pp_makefile (ppf : Format.formatter) (t : t) : unit =
  (* Base definitions. *)
  Format.fprintf ppf "# Base definitions\n\n%!";
  Format.fprintf ppf "TARGET := %a\n%!" T.Target.pp t.target;
  Format.fprintf ppf "LANG := %a\n%!" T.Language.pp t.language;
  Format.fprintf ppf "MODEL := %s\n%!" t.model;
  Format.fprintf ppf "BINARY := %s\n%!" t.binary;
  Format.fprintf ppf "PATCHED_BINARY := %s\n%!" t.patched_binary;
  Format.fprintf ppf "SPACES := %s\n%!" t.spaces;
  Format.fprintf ppf "OGRE := %s\n\n%!" t.ogre;
  (* Patch definitions. *)
  List.iteri t.patches ~f:(fun i p ->
      Format.fprintf ppf "# Definitions for patch %s (%d)\n\n%!" p.name i;
      Format.fprintf ppf "PATCH_%d := %s\n%!" i p.patch;
      Format.fprintf ppf "PATCH_%d_INFO := %s\n%!" i p.patch_info;
      Format.fprintf ppf "PATCH_%d_FUNC := %s\n%!" i p.func_info;
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
      Format.fprintf ppf "$(PATCH_%d_BIR):\n%!" i;
      Format.fprintf ppf "\tvibes-parse \
                          --target $(TARGET) \
                          --patch-filepath $(PATCH_%d) \
                          --patch-info-filepath $(PATCH_%d_INFO) \
                          --function-info-outfile $(PATCH_%d_FUNC) \
                          --bir-outfile $(PATCH_%d_BIR) \
                          --verbose\n\n%!" i i i i;
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
                          --function-info-filepath $(PATCH_%d_FUNC) \
                          --patch-spaces $(SPACES) \
                          --bir-filepath $(PATCH_%d_BIR) \
                          --bir-outfile $(PATCH_%d_BIR_OPT) \
                          --verbose\n\n%!" i i i i;
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
                          $(PATCH_%d_FUNC) \
                          $(PATCH_%d_VIR) \
                          $(PATCH_%d_ASM) \
                          $(PATCHED_BINARY)\n\n%!" i i i i i);
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
    List.map ~f:(sprintf "$(PATCH_%d_ASM)") |>
    String.concat ~sep:" " in
  Format.fprintf ppf ".PHONY: patch\n%!";
  Format.fprintf ppf "patch:\n";
  Format.fprintf ppf "\trm -f $(PATCHED_BINARY)\n%!";
  Format.fprintf ppf "\t$(MAKE) $(PATCHED_BINARY)\n\n%!";
  Format.fprintf ppf "$(PATCHED_BINARY): %s\n%!" asms;
  Format.fprintf ppf "\tvibes-patch \
                      --target $(TARGET) \
                      --language $(LANG) \
                      --binary $(BINARY) \
                      --patch-spaces $(SPACES) \
                      --asm-filepaths %s \
                      --patched-binary $(PATCHED_BINARY) \
                      --ogre $(OGRE) \
                      --verbose\n%!" asms;
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

let dummy_patch_info : Patch_info.t = {
    patch_point = Addr.of_string "0x1234:32";
    patch_size = 4L;
    sp_align = 0;
    patch_vars = [];
  }

let generate_patch_files (i : int) (p : patch) : (unit, KB.conflict) result =
  let* () = Files.write_or_error "" p.patch in
  let* () = Files.write_or_error "" p.constraints in
  let info_data = Format.asprintf "%a" Patch_info.pp dummy_patch_info in
  let* () = Files.write_or_error info_data p.patch_info in
  Ok ()

let generate_files (t : t) : (unit, KB.conflict) result =
  let spaces_data = Format.asprintf "%a" Spaces.pp Spaces.empty in
  let* () = Files.write_or_error spaces_data t.spaces in
  let rec patch i = function
    | [] -> Ok ()
    | p :: rest ->
      let* () = generate_patch_files i p in
      patch (i + 1) rest in
  let* () = patch 0 t.patches in
  Ok ()
