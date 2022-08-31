open Core
open Bap_core_theory

module Assembly = struct

  type block = {
    label : string;
    insns : string list;
  } [@@deriving fields, sexp]

  type t = {
    directives : string list;
    blocks : block list;
  } [@@deriving fields, sexp]

  type printer = Vibes_ir.Types.t -> (t, KB.conflict) result

  let pp_block (ppf : Format.formatter) (blk : block) : unit =
    Format.fprintf ppf "%s:\n%!" blk.label;
    List.iter blk.insns ~f:(Format.fprintf ppf "    %s\n%!")
  
  let pp (ppf : Format.formatter) (asm : t) : unit =
    List.iter asm.directives ~f:(Format.fprintf ppf "%s\n%!");
    List.iter asm.blocks ~f:(Format.fprintf ppf "%a\n%!" pp_block)
  
end
