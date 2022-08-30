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


open Bap.Std
open Core_kernel

open Bap_knowledge
module KB = Knowledge
open Knowledge.Syntax
open Bap_core_theory
open Minizinc_utils

(**
   [mzn_params] Type for interfacing with ocaml code

   [copy] a set of operations that are copy operations. Just give an empty set for now.
   [definer] map from temporary to operation that defines it
   [users] map of temporaries to set of operands that use temporary
   [temp_block] map from temporary to block it belongs to
   [latency] Unimplemented. map from opcode to latency integer
   [preassign] Map from operand to optional preassigned register
   [operand_operation] Map from operands to the operation they belong to
   [congruent] List of congurent operand pairs. For the purposes of cross block operand
              assignment constraints
   [operands] set of all operands
   [temps] Set of of temporaries
   [class_] The role of a given operand for a given opcode
   [ins_map] Map from block tid to `ins` operation id
   [outs_map] Map from block tid to `outs` operation id
   [block_ops] Map from block tid to set of operations in block
*)

type mzn_params = {
  copy : Tid.Set.t;
  definer : Ir.op_var Var.Map.t;
  users : (Ir.op_var list) Var.Map.t;
  temp_block : tid Var.Map.t;
  latency : unit;
  (* width : int Var.Map.t; Vars in Bap have width. Unnecessary? *)
  preassign : var option Var.Map.t;
  operation_opcodes : (Ir.opcode list) Int.Map.t;
  operand_operation : Ir.operation Var.Map.t;
  congruent : (Ir.op_var * Ir.op_var) list;
  operands : Var.Set.t;
  temps : Var.Set.t;
  class_ : (Theory.role Ir.Opcode.Map.t) Var.Map.t;
  ins_map : Ir.operation_id Tid.Map.t;
  outs_map : Ir.operation_id Tid.Map.t;
  block_ops : Ir.operation_id list Tid.Map.t
}

(** [mzn_params_of_vibes_ir] converts a Ir.t subroutine into the intermediate data
    structure mzn_params
    TODO Unimplemented:
       * No copy operations
       * Latency
*)
let mzn_params_of_vibes_ir (sub : Ir.t) : mzn_params =
  {
    copy = Tid.Set.empty;
    definer = Ir.definer_map sub;
    users = Ir.users_map sub;
    temp_block = Ir.temp_blk sub;
    latency = ();
    preassign = Ir.preassign_map sub;
    operation_opcodes = Ir.operation_opcodes sub;
    operand_operation = Ir.operand_operation sub;
    congruent = sub.congruent;
    temps = Ir.all_temps sub;
    operands = Ir.all_operands sub;
    class_ = Ir.op_classes sub;
    ins_map = Ir.ins_map sub;
    outs_map = Ir.outs_map sub;
    block_ops = Ir.block_ops sub
  }

type operand = mzn_enum [@@deriving yojson]
type operation = mzn_enum [@@deriving yojson]
type block = mzn_enum [@@deriving yojson]
type temp = mzn_enum [@@deriving yojson]
type opcode = mzn_enum [@@deriving yojson]
type reg = mzn_enum [@@deriving yojson]
type hvar = mzn_enum [@@deriving yojson]

(* [mzn_params_serial] is a type ready for Minizinc serialization *)

type mzn_params_serial = {
  reg_t : mzn_enum_def;
  opcode_t : mzn_enum_def;
  temp_t : mzn_enum_def;
  hvar_t : mzn_enum_def;
  operand_t : mzn_enum_def;
  operation_t : mzn_enum_def;
  block_t : mzn_enum_def;
  class_t : (operand, (opcode, reg mzn_set) mzn_map) mzn_map;
  operand_operation : (operand, operation) mzn_map;
  definer : (temp, operand) mzn_map;
  users : (temp, operand mzn_set) mzn_map;
  temp_block : (temp, block) mzn_map;
  copy : operation mzn_set;
  width : (temp, int) mzn_map;
  preassign : (operand, reg mzn_set) mzn_map; (* Set should either be empty or have 1 element. *)
  congruent : (operand, operand mzn_set) mzn_map;
  operation_opcodes : (operation, opcode mzn_set) mzn_map;
  latency : (opcode , int) mzn_map;
  number_excluded : int;
  exclude_reg : (int, (temp, reg) mzn_map) mzn_map;
  block_outs : (block, operation) mzn_map;
  block_ins : (block, operation) mzn_map;
  block_operations : (block, operation mzn_set) mzn_map;
  hvars_temps : (hvar, temp mzn_set) mzn_map
}  [@@deriving yojson]


(* [serialization_info] retains auxiliary information useful for interpreting the solution
   returned by minizinc. In particular it retains the ordering of temporaries, operations,
    and operands. This is important because a minizinc map is represented by an array. *)
type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  reg_map : Var.t String.Map.t;
  operations : Int.t list;
  operands : Var.t list;
} [@@deriving equal]

(**
   [sol] is produced by processing [sol_serial]

   [reg] is a mapping from temporaries to registers
   [opcode] is a mapping from operations to opcodes
   [temp] is a mapping from operands to temps
   [active] is a mapping from operations to booleans
   [issue] is a mapping from operations to the issue cycle on which they execute
*)


type sol = Data.sol

type sol_set = Data.sol_set

(* Generic minizinc enumeration builder *)
let key_map ~f:(f : 'a -> 'c)
    (keys : 'b list) (m : ('b, 'a, _) Map.t) : 'c list =
  List.map keys
    ~f:(fun t -> Map.find_exn m t |> f)

let key_map_kb ~f:(f : 'a -> 'c KB.t)
    (keys : 'b list) (m : ('b, 'a, _) Map.t) : 'c list KB.t =
  KB.List.map keys
    ~f:(fun t -> Map.find_exn m t |> f)

(** [serialize_man_params] converts the intermediate structure into the serializable structure
    [mzn_params_serial] and retains [serialization_info]. These two structures are produced at
    the same time to hopefully keep close linkage between them.

    TODO:
       Lift congruences to operands
       Implement latency

*)
let serialize_mzn_params
    ?(congruence : (var * var) list = [])
    ?(exclude_regs : String.Set.t = String.Set.empty)
    (tgt : Theory.target)
    (vir : Ir.t)
    (prev_sols : sol list)
    ~(gpr : Var.Set.t)
    ~(regs : Var.Set.t) : (mzn_params_serial * serialization_info) KB.t =
  let params = mzn_params_of_vibes_ir vir in
  let opcodes = Ir.all_opcodes vir in
  let opcodes_str = List.map opcodes
      ~f:(fun i ->
          Ir.sexp_of_opcode i |>
          Ppx_sexp_conv_lib.Sexp.to_string)
  in
  let blocks = Var.Map.data params.temp_block |> Tid.Set.of_list |> Tid.Set.to_list in
  let operations = Int.Map.keys params.operation_opcodes in
  let operands = Var.Set.to_list params.operands in
  let width t = match Var.typ t with
    | Imm n -> KB.return Float.( (of_int n) / 32.0 |> round_up |> to_int) (* fishy. Use divmod? *)
    | Mem _ -> KB.return 0
    | Unk ->
      Kb_error.(fail @@ Other "Minizinc.serialize_mzn_params: \
                               width unimplemented for Unk")
  in
  let gpr =
    Set.to_list gpr |>
    List.filter ~f:(fun v ->
        not @@ Set.mem exclude_regs @@ Var.name v) |>
    List.map ~f:Var.to_string
  in
  let dummy_reg = Var.create ~is_virtual:false ~fresh:false "dummy_reg" Unk in
  let dummy = dummy_reg |> Var.to_string in
  let regs = Set.to_list regs in
  let regs = List.map ~f:(fun r -> Var.to_string r, r) regs in
  (* Add the dummy register for void/virtual variables *)
  let regs = (dummy, dummy_reg) :: regs in
  let reg_map = String.Map.of_alist_exn regs in
  let regs, _ = List.unzip regs in
  let regs_of_role r =
    if Theory.Role.(r = Register.general) then
      KB.return gpr
    else if Theory.Role.(r = Ir.dummy_role) then
      KB.return [dummy]
    else if Theory.Role.(r = Ir.preassigned) then
      KB.return regs
    else
      Kb_error.(fail @@ Other (
          Format.asprintf "serialize_mzn_params: unsupported register role: %a!"
            Theory.Role.pp r))
  in
  let congruent_temps =
    (* Temps which we may have established congruence for could've been
       optimized away, so ignore them if that was the case. *)
    List.fold congruence ~init:Var.Map.empty ~f:(fun m (t1, t2) ->
        if Set.mem params.temps t1
        && Set.mem params.temps t2 then
          let m = Map.update m t1 ~f:(function
              | None -> Var.Set.singleton t2
              | Some s -> Set.add s t2) in
          Map.update m t2 ~f:(function
              | None -> Var.Set.singleton t1
              | Some s -> Set.add s t1)
        else m) in
  let temps = params.temps |> Var.Set.to_list in
  let temp_names =
    List.map ~f:(fun t -> Var.to_string t) temps
  in
  let hvars = List.filter_map temps ~f:(fun temp ->
      Linear_ssa.orig_name (Var.to_string temp)) in
  let hvars = String.Set.to_list @@ String.Set.of_list hvars in
  let* class_t =
    key_map_kb operands params.class_
      ~f:(fun m -> key_map_kb opcodes m
             ~f:(fun r ->
                 let+ regs = regs_of_role r in
                 mzn_enum_def_of_list regs)) in
  let* width = KB.List.map temps ~f:width in
  let+ () = match regs with
    | _ :: _ -> KB.return ()
    | [] -> Kb_error.fail @@ Other (
        Format.asprintf "Target %a has no registers!" Theory.Target.pp tgt) in
  {
    reg_t = mzn_enum_def_of_list regs;
    opcode_t = mzn_enum_def_of_list opcodes_str;
    temp_t = mzn_enum_def_of_list temp_names;
    (* prefix "hvar_" to (typically) avoid identifier collisions. Minizinc will crash
       if there is a collision *)
    hvar_t = mzn_enum_def_of_list @@ List.map ~f:(fun hvar -> "hvar_" ^ hvar) hvars;
    operand_t = mzn_enum_def_of_list (List.map ~f:Var.to_string operands);
    operation_t = mzn_enum_def_of_list (List.map operations ~f:Int.to_string);
    block_t = List.map ~f:Tid.to_string blocks |>  mzn_enum_def_of_list;
    operand_operation =
      key_map operands params.operand_operation
        ~f:(fun o -> o |> Ir.operation_to_string |> mzn_enum);
    definer =
      key_map temps params.definer
        ~f:(fun t -> t |> Ir.op_var_to_string |> mzn_enum);
    class_t;
    users =
      List.map
        ~f:(fun t ->
            match Var.Map.find params.users t with
            | None -> {set = []}
            | Some operands ->
              {
                set =
                  List.map
                    ~f:(fun o ->
                        Ir.op_var_to_string o |>
                        mzn_enum)
                    operands
              })
        temps;
    temp_block = key_map temps params.temp_block
        ~f:(fun id -> id |> Tid.to_string |> mzn_enum);
    copy  =
      {
        set =
          Tid.Set.to_list params.copy |>
          List.map
            ~f:(fun o -> o |> Tid.to_string |> mzn_enum)
      };
    width;
    preassign = key_map operands params.preassign
        ~f:(function
            | None ->
              { set = [] }
            | Some r ->
              { set = [mzn_enum_of_var r] });
    congruent =
      (* For now, we will compute congruence as a mapping from temps
         to a set of temps. In the full model, we would lift this
         representation to a mapping over operands. *)
      List.map temps ~f:(fun t1 -> {
            set = match Var.typ t1 with
              | Type.Mem _ | Type.Unk -> []
              | Type.Imm _ -> match Map.find congruent_temps t1 with
                | Some s -> Set.to_list s |> List.map ~f:mzn_enum_of_var
                | None -> []
          });
    operation_opcodes = key_map operations params.operation_opcodes
        ~f:(fun ids ->
            { set =
                List.map ids
                  ~f:(fun id ->
                      id |>
                      Ir.sexp_of_opcode |>
                      Ppx_sexp_conv_lib.Sexp.to_string |>
                      mzn_enum)
            });
    latency = List.map ~f:(fun _ -> 10) opcodes; (* TODO *)
    number_excluded = List.length prev_sols;
    exclude_reg =
      List.map prev_sols
        ~f:(fun sol ->
            key_map temps sol.reg ~f:mzn_enum_of_var);
    block_ins = key_map blocks ~f:(fun i -> mzn_enum @@ Int.to_string i) params.ins_map;
    block_outs = key_map blocks ~f:(fun i -> mzn_enum @@ Int.to_string i) params.outs_map;
    block_operations = key_map blocks ~f:(fun ops -> mzn_set_of_list @@
              List.map ops ~f:(fun i -> mzn_enum @@ Int.to_string i)) params.block_ops;
    hvars_temps = List.map hvars ~f:(fun hvar ->
        mzn_set_of_list @@
        List.map ~f:mzn_enum_of_var @@
        List.filter temps ~f:(fun temp ->
          match (Linear_ssa.orig_name @@ Var.to_string temp) with
          | Some oname -> String.equal oname hvar
          | None -> false))
  },
  {
    temps = temps;
    temp_map = List.zip_exn temp_names temps |> String.Map.of_alist_exn;
    reg_map;
    operations = operations;
    operands  = operands
  }

(* [sol_serial] is a datatype for deserialization the minzinc variables via yojson *)
type sol_serial = {
  (* _objective : int;  Optimization is currently not implemented *)
  reg : (temp ,reg) mzn_map;
  opcode : (operation , opcode) mzn_map;
  temp : (operand, temp) mzn_map;
  live : bool list;
  active : bool list;
  issue : int list;
  start_cycle : int list;
  end_cycle : int list;
  _objective : int;
} [@@deriving yojson]







(* FIXME: There is an assumption that the enums are uniquely stringed from to_string
   Minizinc does throw an ertror if there is a replicated enum.
*)


let deserialize_sol (s : sol_serial) (names : serialization_info) : sol =
  let strip_enum (l : mzn_enum list) : string list =
    List.map ~f:(fun t -> t.e) l
  in
  let reg =
    List.map ~f:(fun r -> String.Map.find_exn names.reg_map r.e) s.reg
  in
  {
    reg = List.zip_exn names.temps reg |> Var.Map.of_alist_exn;
    opcode =
      List.map2_exn
        ~f:(fun op opcode -> (op, Sexp.of_string opcode |> Ir.opcode_of_sexp))
        names.operations (strip_enum s.opcode) |> Int.Map.of_alist_exn;
    temp =
      List.map2_exn
        ~f:(fun op temp -> (op , String.Map.find_exn names.temp_map temp))
        names.operands (strip_enum s.temp) |> Var.Map.of_alist_exn;
    active = List.zip_exn names.operations s.active |> Int.Map.of_alist_exn;
    issue = List.zip_exn names.operations s.issue |> Int.Map.of_alist_exn
  }

(** [apply_sol] takes a [sol] and applies it to a [Ir.t].
    The resulting [Ir.t] has it's registers in the preassign field,
    a single temporary in the temps field, and has operations scheduled *)

let apply_sol (vir : Ir.t) (sol : sol) : Ir.t =
  (* Filter inactive operations, and sort operations by issue cycle *)
  let vir = Ir.map_blks vir ~f:(fun b ->
      { b with
        data =
          List.filter b.data
            ~f:(fun o -> Int.Map.find_exn sol.active o.id) |>
          List.sort
            ~compare:(fun o1 o2 -> compare_int
                         (Int.Map.find_exn sol.issue o1.id)
                         (Int.Map.find_exn sol.issue o2.id));
      }
    ) in
  (* Put register and temporary selection into operands *)
  let vir = Ir.map_op_vars vir ~f:(fun o ->
      let temp = Var.Map.find_exn sol.temp o.id in
      let reg = Var.Map.find_exn sol.reg temp in
      { id = o.id ; temps = [temp]; pre_assign = Some reg  }) in
  vir

(* FIXME: this belongs in Ir *)
let delete_empty_blocks vir =
  let open Ir in
  let blks = vir.blks in
  let blks = List.fold blks ~init:[]
      ~f:(fun acc b ->
          if (List.is_empty b.data && List.is_empty b.ctrl)
          then acc
          else b::acc
        )
  in
  {vir with blks = blks}

(** [build_extra_constraints_file] makes a simple minizinc file that
    "include"s (a minizinc keyword) the ordinary model but also appends
    the extra constraints string.
*)
let build_extra_constraints_file ~extra_constraints ~model_filepath : string =
  let wrapper_filepath = Stdlib.Filename.temp_file "vibes-mzn-model" ".mzn" in
  let outc = Out_channel.create wrapper_filepath in
  Out_channel.fprintf outc "include \"%s\";\n%s" model_filepath extra_constraints;
  Out_channel.close outc;
  wrapper_filepath

let run_allocation_and_scheduling
    ?(congruence : (var * var) list = [])
    ?(exclude_regs: String.Set.t = String.Set.empty)
    ?(extra_constraints : string option = None)
    (tgt : Theory.target)
    (prev_sols : sol list)
    (vir : Ir.t)
    ~filepath:(model_filepath : string)
    ~(gpr : Var.Set.t)
    ~(regs : Var.Set.t) : (Ir.t * sol) KB.t =
  Events.(send @@ Info (sprintf "Number of Excluded Solutions: %d\n" (List.length prev_sols)));
  Events.(send @@ Info (sprintf "Orig Ir: %s\n" (Ir.pretty_ir vir)));
  let vir_clean = delete_empty_blocks vir in
  let model_filepath = match extra_constraints with
  | None -> model_filepath
  | Some extra_constraints -> build_extra_constraints_file ~extra_constraints ~model_filepath
  in
  Events.(send @@ Info (sprintf "Minizinc Model Filepath: %s\n" model_filepath));
  let* params, name_maps =
    serialize_mzn_params tgt vir_clean prev_sols
      ~gpr ~regs ~congruence ~exclude_regs in
  (run_minizinc ~model_filepath (mzn_params_serial_to_yojson params)) >>= fun sol_json ->
  let sol_serial = sol_serial_of_yojson sol_json in
  let sol = match sol_serial with
    | Ok sol_serial -> KB.return (deserialize_sol sol_serial name_maps)
    | Error msg -> Kb_error.fail (Kb_error.Minizinc_deserialization msg) in
  sol >>= fun sol ->
  let vir' = apply_sol vir sol in
  Events.(send @@ Info (sprintf "Solved Ir: %s\n" (Ir.pretty_ir vir')));
  KB.return (vir', sol)
