open Core
open Bap.Std
open Monads.Std
open Bap_core_theory

module T = Theory
module Ir = Vibes_ir.Types
module Helpers = Vibes_bir.Helpers

module Serializer = struct

  let serialize_id (id : Ir.id) : Sexp.t =
    Atom (Int.to_string id)

  let serialize_opcode (o : Ir.opcode) : Sexp.t =
    Atom o

  let serialize_opvar (v : Ir.Opvar.t) : Sexp.t = List [
      Atom "opvar";
      serialize_id v.id;
      List (List.map v.temps ~f:Bir.serialize_var);
      List (List.map (Option.to_list v.preassign) ~f:Bir.serialize_var);
    ]

  let serialize_operand : Ir.Operand.t -> Sexp.t = function
    | Var v -> List [Atom "var"; serialize_opvar v]
    | Const c -> List [Atom "const"; Atom (Word.to_string c)]
    | Label l -> List [Atom "label"; Atom (Tid.to_string l)]
    | Void v -> List [Atom "void"; serialize_opvar v]
    | Offset o -> List [Atom "offset"; Atom (Word.to_string o)]

  let serialize_operation (o : Ir.Operation.t) : Sexp.t = List [
      serialize_id o.id;
      List [Atom "lhs"; List (List.map o.lhs ~f:serialize_operand)];
      List [Atom "opcodes"; List (List.map o.opcodes ~f:serialize_opcode)];
      List [Atom "optional"; Atom (Bool.to_string o.optional)];
      List [Atom "operands"; List (List.map o.operands ~f:serialize_operand)];
    ]

  let serialize_ins (o : Ir.Operation.t) : Sexp.t = List [
      serialize_id o.id;
      List (List.map o.lhs ~f:serialize_operand);
    ]

 let serialize_outs (o : Ir.Operation.t) : Sexp.t = List [
      serialize_id o.id;
      List (List.map o.operands ~f:serialize_operand);
    ] 
  
  let serialize_block (b : Ir.Block.t) : Sexp.t = List [
      Atom (Tid.to_string b.tid);
      List [Atom "data"; List (List.map b.data ~f:serialize_operation)];
      List [Atom "ctrl"; List (List.map b.ctrl ~f:serialize_operation)];
      List [Atom "ins"; serialize_ins b.ins];
      List [Atom "outs"; serialize_outs b.outs];
      List [Atom "frequency"; Atom (Int.to_string b.frequency)];
    ]

  let serialize_congruence ((v, c) : var * Var.Set.t) : Sexp.t = List [
      Bir.serialize_var v;
      List (Set.to_list c |> List.map ~f:Bir.serialize_var);
    ]

  let serialize_congruences (c : Var.Set.t Var.Map.t) : Sexp.t = List [
      Atom "congruences";
      List (Map.to_alist c |> List.map ~f:serialize_congruence);
    ]

  let serialize_ir (ir : Ir.t) : Sexp.t = List [
      Atom "program";
      List (List.map ir.blks ~f:serialize_block);
      serialize_congruences ir.congruences;
    ]

end

module Deserialize = struct

  module Env = struct

    type t = {
      vars : var String.Map.t;
      tids : tid String.Map.t;
      ids : Ir.id String.Map.t;
    }

    let empty : t = {
      vars = String.Map.empty;
      tids = String.Map.empty;
      ids = String.Map.empty;
    }

  end

  include Monad.State.T1(Env)(KB)
  include Monad.State.Make(Env)(KB)

  let fail (err : KB.conflict) : 'a t = lift @@ KB.fail err

  let lookup_var (s : string) : var option t =
    gets @@ fun {vars; _} -> Map.find vars s

  let lookup_tid (s : string) : tid option t =
    gets @@ fun {tids; _} -> Map.find tids s

  let lookup_id (s : string) : Ir.id option t =
    gets @@ fun {ids; _} -> Map.find ids s 

  let add_var (s : string) (v : var) : unit t =
    update @@ fun env -> {
      env with vars = Map.set env.vars ~key:s ~data:v;
    }

  let provide_alias (s : string) (t : tid) : unit t =
    let alias = Set.singleton (module String) s in
    lift @@ KB.provide T.Label.aliases t alias

  let add_tid (s : string) (t : tid) : unit t =
    let* () = provide_alias s t in
    update @@ fun env -> {
      env with tids = Map.set env.tids ~key:s ~data:t;
    }

  let add_id (s : string) (id : Ir.id) : unit t =
    update @@ fun env -> {
      env with ids = Map.set env.ids ~key:s ~data:id;
    }

  let deserialize_tid (s : string) : tid t =
    let* tid = lookup_tid s in
    match tid with
    | Some tid -> !!tid
    | None ->
      let* tid = lift @@ T.Label.fresh in
      let+ () = add_tid s tid in
      tid

  let deserialize_id (s : string) : Ir.id t =
    let* id = lookup_id s in
    match id with
    | Some id -> !!id
    | None ->
      let id = Ir.fresh_id () in
      let+ () = add_id s id in
      id

  let deserialize_int (s : string) : int t =
    match int_of_string_opt s with
    | Some i -> !!i
    | None ->
      let msg = Format.sprintf "Expected int, but got '%s'" s in
      fail @@ Errors.Invalid_vir msg

  let deserialize_addr_size : Sexp.t -> addr_size t = function
    | Atom raw as sexp -> begin
        let* i = deserialize_int raw in
        match Size.of_int_opt i with
        | Some `r32 -> !!`r32
        | Some `r64 -> !!`r64
        | None | Some _ ->
          let msg = Format.asprintf
              "Expected Bap.Std.addr_size, but got '%a'"
              Sexp.pp sexp in
          fail @@ Errors.Invalid_vir msg
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected integer size, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_size : Sexp.t -> size t = function
    | Atom raw as sexp -> begin
        let* i = deserialize_int raw in
        match Size.of_int_opt i with
        | Some size -> !!size
        | None ->
          let msg = Format.asprintf
              "Expected Bap.Std.size, but got '%a'"
              Sexp.pp sexp in
          fail @@ Errors.Invalid_vir msg
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected integer size, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_bool (s : string) : bool t =
    match bool_of_string_opt s with
    | Some i -> !!i
    | None ->
      let msg = Format.sprintf "Expected bool, but got '%s'" s in
      fail @@ Errors.Invalid_vir msg

  let deserialize_typ : Sexp.t -> typ t = function
    | List [Atom "imm"; Atom n] ->
      let+ n = deserialize_int n in
      Type.Imm n
    | List [Atom "mem"; a; b] ->
      let* a = deserialize_addr_size a in
      let+ b = deserialize_size b in
      Type.Mem (a, b)
    | List [Atom "unk"] -> !!Type.Unk
    | sexp ->
      let msg = Format.asprintf
          "Expected Bap.Std.typ, but got '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let same_typ (t : typ) (v : var) : bool =
    Type.equal t @@ Var.typ v

  let same_virt (b : bool) (v : var) : bool =
    Bool.equal b @@ Var.is_virtual v

  let deserialize_var : Sexp.t -> var t = function
    | List [Atom name; Atom idx; typ; Atom virt] -> begin
        let* var = lookup_var name in
        let* idx = deserialize_int idx in
        let* typ = deserialize_typ typ in
        let* is_virtual = deserialize_bool virt in
        match var with
        | Some v when not @@ same_typ typ v ->
          let msg = Format.asprintf
              "Expected type '%a' for var '%s', but got '%a'"
              Type.pp (Var.typ v) name Type.pp typ in
          fail @@ Errors.Invalid_vir msg
        | Some v when not @@ same_virt is_virtual v ->
          let s b = if b then "virtual" else "physical" in
          let msg = Format.asprintf
              "Expected var '%s' to be %s, but got %s"
              name (s @@ Var.is_virtual v) (s is_virtual) in
          fail @@ Errors.Invalid_vir msg
        | Some v when Var.index v = idx -> !!v
        | x ->
          let s = Helpers.sort_of_typ typ in
          let* v =
            if is_virtual then lift @@ T.Var.fresh s
            else !!(T.Var.define s name) in
          let v = Var.(with_index (reify v) idx) in
          (* If the var is already in our env, but this one
             has a different index, then don't re-add it to
             the env. We've already verified that its type
             is consistent with the previous definition. *)
          let+ () =
            if Option.is_some x then !!()
            else add_var name v in
          v 
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected '(string int typ bool)', but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_word (sexp : Sexp.t) : word t =
    let msg = Format.asprintf
        "Expected 'Bap.Std.word', but got: '%a'"
        Sexp.pp sexp in
    match sexp with
    | Atom w -> begin
        try !!(Word.of_string w)
        with _ -> fail @@ Errors.Invalid_vir msg
      end
    | _ -> fail @@ Errors.Invalid_vir msg

  let deserialize_opvar : Sexp.t -> Ir.Opvar.t t = function
    | List [Atom "opvar"; Atom id; List temps; List []] ->
      let* id = deserialize_id id in
      let+ temps = List.map temps ~f:deserialize_var in
      Ir.Opvar.Fields.create ~id ~temps ~preassign:None
    | List [Atom "opvar"; Atom id; List temps; List [preassign]] ->
      let* id = deserialize_id id in
      let* temps = List.map temps ~f:deserialize_var in
      let+ pre = deserialize_var preassign in
      Ir.Opvar.Fields.create ~id ~temps ~preassign:(Some pre)
    | sexp ->
      let msg = Format.asprintf
          "Expected opvar, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_operand : Sexp.t -> Ir.Operand.t t = function
    | List [Atom "var"; v] ->
      let+ v = deserialize_opvar v in
      Ir.Operand.Var v
    | List [Atom "const"; c] ->
      let+ c = deserialize_word c in
      Ir.Operand.Const c
    | List [Atom "label"; Atom l] ->
      let+ l = deserialize_tid l in
      Ir.Operand.Label l
    | List [Atom "void"; v] ->
      let+ v = deserialize_opvar v in
      Ir.Operand.Void v
    | List [Atom "offset"; o] ->
      let+ o = deserialize_word o in
      Ir.Operand.Offset o
    | sexp ->
      let msg = Format.asprintf
          "Expected operand, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_operation : Sexp.t -> Ir.Operation.t t = function
    | List [
        Atom id;
        List [Atom "lhs"; List lhs];
        List [Atom "opcodes"; List opcodes];
        List [Atom "optional"; Atom optional];
        List [Atom "operands"; List operands];
      ] ->
      let* id = deserialize_id id in
      let* lhs = List.map lhs ~f:deserialize_operand in
      let* opcodes = List.map opcodes ~f:(function
          | Atom opcode -> !!opcode
          | sexp ->
            let msg = Format.asprintf
                "Expected opcode, but got: '%a'"
                Sexp.pp sexp in
            fail @@ Errors.Invalid_vir msg) in
      let* optional = deserialize_bool optional in
      let+ operands = List.map operands ~f:deserialize_operand in
      Ir.Operation.Fields.create ~id ~lhs ~opcodes ~optional ~operands
    | sexp ->
      let msg = Format.asprintf
          "Expected operation, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_ins : Sexp.t -> Ir.Operation.t t = function
    | List [Atom id; List ops] ->
      let* id = deserialize_id id in
      let+ lhs = List.map ops ~f:deserialize_operand in
      Ir.Operation.Fields.create ~id ~lhs
        ~opcodes:[]
        ~optional:false
        ~operands:[]
    | sexp ->
      let msg = Format.asprintf
          "Expected ins, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_outs : Sexp.t -> Ir.Operation.t t = function
    | List [Atom id; List ops] ->
      let* id = deserialize_id id in
      let+ operands = List.map ops ~f:deserialize_operand in
      Ir.Operation.Fields.create ~id ~operands
        ~lhs:[]
        ~opcodes:[]
        ~optional:false
    | sexp ->
      let msg = Format.asprintf
          "Expected outs, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg
  
  let deserialize_block : Sexp.t -> Ir.Block.t t = function
    | List [
        Atom tid;
        List [Atom "data"; List data];
        List [Atom "ctrl"; List ctrl];
        List [Atom "ins"; ins];
        List [Atom "outs"; outs];
        List [Atom "frequency"; Atom frequency];
      ] ->
      let* tid = deserialize_tid tid in
      let* data = List.map data ~f:deserialize_operation in
      let* ctrl = List.map ctrl ~f:deserialize_operation in
      let* ins = deserialize_ins ins in
      let* outs = deserialize_outs outs in
      let+ frequency = deserialize_int frequency in
      Ir.Block.Fields.create ~tid ~data ~ctrl ~ins ~outs ~frequency
    | sexp ->
      let msg = Format.asprintf
          "Expected block, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_congruence : Sexp.t -> (var * Var.Set.t) t = function
    | List [v; List c] ->
      let* v = deserialize_var v in
      let+ c = List.map c ~f:deserialize_var in
      v, Var.Set.of_list c
    | sexp ->
      let msg = Format.asprintf
          "Expected congruence, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_congruences : Sexp.t -> Var.Set.t Var.Map.t t = function
    | List [Atom "congruences"; List c] -> begin
        let* c = List.map c ~f:deserialize_congruence in
        match Var.Map.of_alist c with
        | `Ok m -> !!m
        | `Duplicate_key v ->
          let msg = Format.asprintf
              "Duplicate key '%a' in congruences"
              Var.pp v in
          fail @@ Errors.Invalid_vir msg
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected congruences, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

  let deserialize_ir : Sexp.t -> Ir.t t = function
    | List [Atom "program"; List blks; congruences] ->
      let* blks = List.map blks ~f:deserialize_block in
      let+ congruences = deserialize_congruences congruences in
      Ir.{blks; congruences}
    | sexp ->
      let msg = Format.asprintf
          "Expected VIBES IR, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_vir msg

end

let serialize : Ir.t -> Sexp.t = Serializer.serialize_ir

let deserialize (sexp : Sexp.t) : Ir.t KB.t =
  let open Deserialize in
  let open KB.Syntax in
  let+ ir, _ = run (deserialize_ir sexp) Env.empty in
  ir
