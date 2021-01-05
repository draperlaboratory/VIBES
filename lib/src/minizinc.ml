open Bap.Std
open Core_kernel

module VIR = Vibes_ir

open Bap_knowledge
module KB = Knowledge
open Knowledge.Syntax


(**
   [mzn_params] Type for interfacing with ocaml code

   [copy] a set of operations that are copy operations. Just give an empty set for now.
   [definer] map from temporary to operation that defines it
   [users] map of temporaries to set of operands that use temporary
   [temp_block] map from temporary to block it belongs to
   [latency] Unimplemented. map from insn to latency integer
   [preassign] Map from operand to optional preassigned register
   [operand_operation] Map from operands to the operation they belong to
   [congruent] List of congurent operand pairs. For the purposes of cross block operand
              assignment constraints
   [operands] set of all operands
   [temps] Set of of temporaries
*)

type mzn_params = {
  copy : Tid.Set.t;
  definer : VIR.op_var Var.Map.t;
  users : (VIR.op_var list) Var.Map.t;
  temp_block : tid Var.Map.t;
  latency : unit;
  (* width : int Var.Map.t; Vars in Bap have width. Unnecessary? *)
  preassign : string Var.Map.t;
  operation_insns : (Vibes_ir.insn list) Tid.Map.t;
  operand_operation : VIR.operation Var.Map.t;
  congruent : (VIR.op_var * VIR.op_var) list ;
  operands : Var.Set.t;
  temps : Var.Set.t
}

(** [mzn_params_of_vibes_ir] converts a VIR.t subroutine into the intermediate data
    structure mzn_params
    TODO Unimplemented:
       * No copy instructions
       * Latency
       * Preassignment
*)
let mzn_params_of_vibes_ir (sub : VIR.t) : mzn_params =
  {
    copy = Tid.Set.empty;
    definer = VIR.definer_map sub;
    users = VIR.users_map sub;
    temp_block = VIR.temp_blk sub;
    latency = ();
    preassign = Var.Map.empty;
    operation_insns = VIR.operation_insns sub;
    operand_operation =  VIR.operand_operation sub;
    congruent =  sub.congruent;
    temps = VIR.all_temps sub;
    operands = VIR.all_operands sub;
  }


(* Convenience types for minizinc serialization. At this point nearly everything
   becomes stringly typed. The {set :} and {e : } wrappers produce the correct json
   for serialization to minzinc *)
type 'a mznset = {set : 'a list}  [@@deriving yojson]
type ('a ,'b) mznmap = 'b list

(* Phantom types make yojson_deriving produce function with unused variables.
   This sets off a warning *)
let mznmap_of_yojson = fun _ -> [%of_yojson: 'b list]
let mznmap_to_yojson = fun _ -> [%to_yojson: 'b list]

type mzn_enum = {e : string} [@@deriving yojson]
type mzn_enum_def = mzn_enum mznset [@@deriving yojson] (* https://github.com/MiniZinc/libminizinc/issues/441 *)
type operand = mzn_enum [@@deriving yojson]
type operation = mzn_enum [@@deriving yojson]
type block = mzn_enum [@@deriving yojson]
type temp = mzn_enum [@@deriving yojson]
type insn = mzn_enum [@@deriving yojson]
type reg = mzn_enum [@@deriving yojson]

let mzn_enum (x : string) : mzn_enum = {e = x}
let mzn_enum_def_of_list (tags : string list) : mzn_enum_def = {set = List.map ~f:mzn_enum tags}

(* [mzn_params_serial] is a type ready for Minizinc serialization *)

type mzn_params_serial = {
  reg_t : mzn_enum_def;
  insn_t : mzn_enum_def;
  temp_t : mzn_enum_def;
  operand_t : mzn_enum_def;
  operation_t : mzn_enum_def;
  block_t : mzn_enum_def;
  class_t : mzn_enum_def;
  operand_operation : (operand, operation) mznmap;
  definer : (temp, operand) mznmap;
  users : (temp, operand mznset) mznmap;
  temp_block : (temp, block) mznmap;
  copy : operation mznset;
  width : (temp, int) mznmap;
  preassign : (operand, reg mznset) mznmap; (* Set should either be empty or have 1 element. *)
  congruent : (operand, operand mznset) mznmap;
  operation_insns : (operation, insn mznset) mznmap;
  latency : (insn , int) mznmap
}  [@@deriving yojson]


(* [serialization_info] retains auxiliary information useful for interpreting the solution
   returned by minizinc. In particular it retains the ordering of temporaries, operations,
    and operands. This is important because a minizinc map is represented by an array. *)
type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  operations : Tid.t list;
  operands : Var.t list;
}

(** [serialize_man_params] converts the intermediate structure into the serializable structure
    [mzn_params_serial] and retains [serialization_info]. These two structures are produced at
    the same time to hopefully keep close linkage between them.

    TODO:
       Implement congruence
       Implement preassignment
       Implement latency

*)
let serialize_mzn_params (vir : Vibes_ir.t) : mzn_params_serial * serialization_info =
  let params = mzn_params_of_vibes_ir vir in
  let temps = Var.Set.to_list params.temps in
  let temp_names = List.map ~f:(fun t -> Var.sexp_of_t t |> Sexp.to_string) temps in
  let insns = Tid.Map.data params.operation_insns |> List.concat_map
                ~f:(fun is -> List.map is ~f:(fun i -> Vibes_ir.sexp_of_insn i
                                                       |>  Ppx_sexp_conv_lib.Sexp.to_string))
              |> String.Set.of_list |> String.Set.to_list in
  let blocks = Var.Map.data params.temp_block |> Tid.Set.of_list |> Tid.Set.to_list in
  let operations = Tid.Map.keys params.operation_insns in
  let operands = Var.Set.to_list params.operands(* Var.Map.keys params.operand_operation *) in
  let width t = match Var.typ t with
    | Imm n -> Float.( (of_int n) / 32.0 |> round_up |> to_int) (* fishy. Use divmod? *)
    | _ -> failwith "width unimplemented for Mem or Unk"
  in
  {
    reg_t = mzn_enum_def_of_list (List.map
                                    ~f:(fun r -> ARM.sexp_of_gpr_reg r |> Sexp.to_string)
                                    ARM.all_of_gpr_reg);
    insn_t = mzn_enum_def_of_list insns;
    temp_t =  mzn_enum_def_of_list temp_names;
    operand_t =  mzn_enum_def_of_list (List.map ~f:Var.to_string operands);
    operation_t = mzn_enum_def_of_list (List.map operations ~f:Tid.to_string);
    block_t = List.map ~f:Tid.to_string blocks |>  mzn_enum_def_of_list;
    class_t = {set = [{e = "unimplemented_class"}]};
    operand_operation = List.map ~f:(fun t -> Var.Map.find_exn params.operand_operation t
                                              |> VIR.operation_to_string |> mzn_enum ) operands;
    definer = List.map ~f:(fun t -> Var.Map.find_exn params.definer t |> VIR.op_var_to_string
                                    |> mzn_enum) temps;
    users = List.map ~f:(fun t -> match Var.Map.find params.users t with
        | None -> {set = []}
        | Some operands -> {
            set = List.map
                ~f:(fun o -> VIR.op_var_to_string o |> mzn_enum) operands} )
        temps;
    temp_block = List.map temps
        ~f:(fun t -> Var.Map.find_exn params.temp_block t |> Tid.to_string |> mzn_enum);
    copy  = {set = Tid.Set.to_list params.copy |>
                   List.map ~f:(fun o -> Tid.to_string o |> mzn_enum)
            };
    width = List.map ~f:width temps;
    preassign = List.map ~f:(fun _ -> {set = []} ) operands ; (* TODO *)
    congruent = List.map ~f:(fun _ -> {set = []} ) operands ; (* TODO *)
    operation_insns = List.map ~f:(fun o ->
        {set = Tid.Map.find_exn params.operation_insns o
               |> List.map ~f:(fun i -> Vibes_ir.sexp_of_insn i
                                        |> Ppx_sexp_conv_lib.Sexp.to_string |> mzn_enum )
        }) operations;
    latency = List.map ~f:(fun _ -> 10) insns (* TODO *)
  },
  {
    temps = temps;
    temp_map = List.zip_exn temp_names temps |> String.Map.of_alist_exn;
    operations = operations;
    operands  = operands
  }

(* [sol_serial] is a datatype for deserialization the minzinc variables via yojson *)
type sol_serial = {
  (* _objective : int ;  Optimization is currently not implemented *)
  reg : (temp ,reg) mznmap;
  insn : (operation , insn) mznmap;
  temp : (operand, temp) mznmap;
  live : bool list;
  active : bool list;
  issue : int list;
  start_cycle : int list;
  end_cycle : int list
} [@@deriving yojson]

(**
   [sol] is produced by processing [sol_serial]

   [reg] is a mapping from temporaries to registers
   [insn] is a mapping from operations to instructions
   [temp] is a mapping from operands to temps
   [active] is a mapping from operations to booleans
   [issue] is a mapping from operations to the issue cycle on which they execute
*)

type sol = {
  reg : ARM.gpr_reg Var.Map.t;
  insn : Vibes_ir.insn Tid.Map.t;
  temp : Var.t Var.Map.t;
  active : bool Tid.Map.t;
  issue : int Tid.Map.t;
}





(* FIXME: There is an assumption that the enums are uniquely stringed from to_string
   Minizinc does throw an ertror if there is a replicated enum.
*)


let deserialize_sol (s : sol_serial) (names : serialization_info) : sol =
  let strip_enum (l : mzn_enum list) : string list = List.map ~f:(fun t -> t.e) l in
  let reg = List.map ~f:(fun r -> Sexp.of_string r.e |> ARM.gpr_reg_of_sexp) s.reg in
  {
    reg =  List.zip_exn names.temps reg |> Var.Map.of_alist_exn;
    insn = List.map2_exn
        ~f:(fun op insn -> (op ,  Sexp.of_string insn |> Vibes_ir.insn_of_sexp ))
        names.operations
        (strip_enum s.insn)
           |> Tid.Map.of_alist_exn;
    temp = List.map2_exn
        ~f:(fun op temp -> (op , String.Map.find_exn names.temp_map temp ))
        names.operands
        (strip_enum s.temp)
           |> Var.Map.of_alist_exn;
    active = List.zip_exn names.operations s.active |> Tid.Map.of_alist_exn;
    issue = List.zip_exn names.operations s.issue |> Tid.Map.of_alist_exn
  }

(** [apply_sol] takes a [sol] and applies it to a [VIR.t].
    The resulting [VIR.t] has it's registers in the preassign field,
    a single temporary in the temps field, and has operations scheduled *)

let apply_sol (vir : VIR.t) (sol : sol) : VIR.t =
  (* Filter inactive operations, and sort operations by issue cycle *)
  let vir = VIR.map_blks vir ~f:(fun b ->
      { id = b.id ;
        operations = List.filter ~f:(fun o -> Tid.Map.find_exn sol.active o.id) b.operations |>
                     List.sort ~compare:(fun o1 o2 -> compare_int (Tid.Map.find_exn sol.issue o1.id )
                                            (Tid.Map.find_exn sol.issue o2.id ) )  ;
        ins = b.ins ;
        outs = b.outs ;
        frequency = b.frequency }
    ) in
  (* Put register and temporary selection into operands *)
  let vir = VIR.map_op_vars vir ~f:(fun o ->
      let temp = Var.Map.find_exn sol.temp o.id in
      let reg = Var.Map.find_exn sol.reg temp in
      { id = o.id  ; temps = [temp] ; pre_assign = Some reg  } ) in
  (*  Set instruction field of operation *)
  let vir = VIR.map_operations vir ~f:(fun o ->
      {
        id = o.id;
        lhs = o.lhs;
        insns = [ Tid.Map.find_exn sol.insn o.id ];
        optional =  not (Tid.Map.find_exn sol.active o.id);
        operands = o.operands;
      } ) in
  vir

let model = Model.model

let run_minizinc (vir : VIR.t) : VIR.t KB.t =
  let model_filename =
    Stdlib.Filename.temp_file "vibes-model" ".mzn" in
  let params_filename =
    Stdlib.Filename.temp_file "vibes-mzn-params" ".json" in
  let solution_filename =
    Stdlib.Filename.temp_file "vibes-mzn-sol" ".json" in
  Out_channel.with_file model_filename
    ~f:(fun out -> Out_channel.fprintf out "%s\r\n" model);
  Events.(send @@ Info (sprintf "Paramfile: %s\n" params_filename));
  Events.(send @@ Info (sprintf "Orig VIR: %s\n" (VIR.pretty_ir vir)));
  let params, name_maps = serialize_mzn_params vir in
  Yojson.Safe.to_file params_filename (mzn_params_serial_to_yojson params);
  let minizinc_args = ["--output-mode"; "json";
                       "-o"; solution_filename ;
                       "--output-objective" ;
                       "-d" ; params_filename ;
                       "--soln-sep"; "\"\"";  (* Suppress some unwanted annotations *)
                       "--search-complete-msg" ;"\"\"" ;
                       model_filename ] in
  Utils.run_process_exn "minizinc" minizinc_args >>= fun () ->
  let sol_serial = Yojson.Safe.from_file solution_filename |> sol_serial_of_yojson  in
  let sol = match sol_serial with
    | Ok sol_serial -> KB.return (deserialize_sol sol_serial name_maps)
    | Error msg -> Errors.fail (Errors.Minizinc_deserialization msg) in

  sol >>= fun sol ->
  let vir' = apply_sol vir sol in
  Events.(send @@ Info (sprintf "Solved VIR: %s\n" (VIR.pretty_ir vir)));
  KB.return vir'



