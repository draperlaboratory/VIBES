open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Serializers = Vibes_serializers
module C_toolkit = Vibes_c_toolkit
module Patch_info = Vibes_patch_info.Types
module Hvar = Vibes_higher_vars.Higher_var
module Bir_helpers = Vibes_bir.Helpers
module Tags = Vibes_bir.Tags

open KB.Syntax

let parse_c_code (raw_code : string) : (Types.ast, KB.conflict) result =
  Log.send "Parsing C code";
  match C_toolkit.Parse_c.parse raw_code with
  | Error _ as e -> e
  | Ok ast as a ->
    let s = C_toolkit.C_utils.print_c Cprint.print_def ast in
    Log.send "Parsed:\n%s" s;
    a

let mark_jmps (blk : blk term) : blk term KB.t =
  Term.KB.map jmp_t blk ~f:(fun jmp -> match Jmp.alt jmp with
      | None -> !!jmp
      | Some alt -> match Jmp.resolve alt with
        | Second _ -> !!jmp
        | First tid ->
          let* name = KB.collect T.Label.name tid in
          let+ addr = KB.collect T.Label.addr tid in
          let jmp = match name with
            | Some name -> Term.set_attr jmp Tags.name_dest name
            | None -> jmp in
          let jmp = match addr with
            | Some addr -> Term.set_attr jmp Tags.addr_dest addr
            | None -> jmp in
          jmp)

let mark_args (args : Var.Set.t) (blk : blk term) : blk term =
  if Set.is_empty args then blk
  else Term.map def_t blk ~f:(fun def ->
      let lhs = Def.lhs def in
      if Set.mem args lhs then
        Term.set_attr def Tags.argument ()
      else def)

let compile
    (name : string)
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : (sub term, KB.conflict) result =
  try
    let result = Toplevel.var "vibes-parse" in
    Toplevel.put result begin
      let* label = Compile.to_core ast target hvars in
      let* sem = KB.collect T.Semantics.slot label in
      Log.send "Semantics:\n%a" KB.Value.pp sem;
      let* blks = Blk.KB.from_insns [sem] in
      let* blks = KB.List.map blks ~f:(fun blk ->
          let tid = Term.tid blk in
          let* args = KB.collect C_toolkit.Core_c.call_with_args tid in
          let+ blk = mark_jmps blk in
          mark_args args blk) in
      let+ sub = Bir_helpers.create_sub name blks in
      Log.send "Lifted BIR program:\n%a" Sub.pp sub;
      sub
    end;
    Ok (Toplevel.get result)
  with Toplevel.Conflict err -> Error err

let run
    ~(target : string)
    ~(patch_info_filepath : string)
    ~(patch_filepath : string)
    ~(bir_outfile : string) : (unit, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  Log.send "Vibes_parse.Runner.run '%s' '%s' '%s' '%s'"
    target patch_info_filepath patch_filepath bir_outfile;
  Log.send "Loading patch-info";
  let* patch_info = Patch_info.from_file patch_info_filepath in
  let hvars = patch_info.patch_vars in
  let* target = Utils.Core_theory.get_target target in
  let* raw_code = Utils.Files.get_file_contents patch_filepath in
  let* ast = parse_c_code raw_code in
  let bir_name = Filename.basename bir_outfile in
  let* bir = compile bir_name ast target hvars in
  let bir_sexp = Serializers.Bir.serialize bir in
  let bir_data = Sexp.to_string_hum bir_sexp in
  Log.send "Writing serialized BIR";
  let finalized_bir = Format.sprintf "%s\n" bir_data in
  Utils.Files.write_or_error finalized_bir bir_outfile
