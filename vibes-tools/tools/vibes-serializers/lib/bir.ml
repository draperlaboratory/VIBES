open Core
open Bap.Std
open Bap_core_theory
open Monads.Std

module T = Theory
module Helpers = Vibes_bir.Helpers
module Tags = Vibes_bir.Tags

module Serializer = struct

  module Env = struct

    type t = {
      attrs : dict String.Map.t;
    }

    let empty = {
      attrs = String.Map.empty;
    }

  end

  include Monad.State.T1(Env)(Monad.Ident)

  module M = Monad.State.Make(Env)(Monad.Ident)

  let update_attrs s dict =
    M.update @@ fun env -> {
      attrs = Map.set env.attrs ~key:s ~data:dict;
    }

  open M.Let
  open M.Syntax

  let serialize_size (sz : size) : Sexp.t =
    Atom (Int.to_string @@ Size.in_bits sz)

  let serialize_typ : typ -> Sexp.t = function
    | Imm n -> List [Atom "imm"; Atom (Int.to_string n)]
    | Mem (a, b) -> List [
        Atom "mem";
        serialize_size (a :> size);
        serialize_size b;
      ]
    | Unk -> Atom "unk"

  let serialize_endianness : endian -> Sexp.t = function
    | LittleEndian -> Atom "LittleEndian"
    | BigEndian -> Atom "BigEndian"

  let serialize_cast : cast -> Sexp.t = function
    | UNSIGNED -> Atom "UNSIGNED"
    | SIGNED -> Atom "SIGNED"
    | HIGH -> Atom "HIGH"
    | LOW -> Atom "LOW"

  let serialize_unop : unop -> Sexp.t = function
    | NEG -> Atom "NEG"
    | NOT -> Atom "NOT"

  let serialize_binop : binop -> Sexp.t = function
    | PLUS -> Atom "PLUS"
    | MINUS -> Atom "MINUS"
    | TIMES -> Atom "TIMES"
    | DIVIDE -> Atom "DIVIDE"
    | SDIVIDE -> Atom "SDIVIDE"
    | MOD -> Atom "MOD"
    | SMOD -> Atom "SMOD"
    | LSHIFT -> Atom "LSHIFT"
    | RSHIFT -> Atom "RSHIFT"
    | ARSHIFT -> Atom "ARSHIFT"
    | AND -> Atom "AND"
    | OR -> Atom "OR"
    | XOR -> Atom "XOR"
    | EQ -> Atom "EQ"
    | NEQ -> Atom "NEQ"
    | LT -> Atom "LT"
    | LE -> Atom "LE"
    | SLT -> Atom "SLT"
    | SLE -> Atom "SLE"

  let serialize_var (v : var) : Sexp.t = List [
      Atom Var.(name @@ base v);
      Atom (Int.to_string @@ Var.index v);
      serialize_typ @@ Var.typ v;
      Atom (Bool.to_string @@ Var.is_virtual v);
    ]

  let rec serialize_exp : exp -> Sexp.t = function
    | Load (e1, e2, endianness, size) -> List [
        Atom "load";
        serialize_exp e1;
        serialize_exp e2;
        serialize_endianness endianness;
        serialize_size size
      ]
    | Store (e1, e2, e3, endianness, size) -> List [
        Atom "store";
        serialize_exp e1;
        serialize_exp e2;
        serialize_exp e3;
        serialize_endianness endianness;
        serialize_size size
      ]
    | BinOp (op, e1, e2) -> List [
        Atom "binop";
        serialize_binop op;
        serialize_exp e1;
        serialize_exp e2;
      ]
    | UnOp (op, e) ->
      let e = serialize_exp e in
      let op = serialize_unop op in
      List [Atom "unop"; op; e]
    | Var v -> List [Atom "var"; serialize_var v]
    | Int w -> List [Atom "int"; Atom (Word.to_string w)]
    | Cast (cast, width, e) -> List [
        Atom "cast";
        serialize_cast cast;
        Atom (Int.to_string width);
        serialize_exp e
      ]
    | Ite (e1, e2, e3) -> List [
        Atom "ite";
        serialize_exp e1;
        serialize_exp e2;
        serialize_exp e3;
      ]
    | Extract (hi, lo, e) -> List [
        Atom "extract";
        Atom (Int.to_string hi);
        Atom (Int.to_string lo);
        serialize_exp e
      ]
    | Concat (e1, e2) -> List [
        Atom "concat";
        serialize_exp e1;
        serialize_exp e2;
      ]
    | Let (v, x, y) -> List [
        Atom "let";
        serialize_var v;
        serialize_exp x;
        serialize_exp y;
      ]
    | Unknown (s, t) -> List [
        Atom "unknown";
        Atom s;
        serialize_typ t;
      ]

  let serialize_def (def : def term) : Sexp.t t =
    let tid = Tid.to_string @@ Term.tid def in
    let lhs = serialize_var @@ Def.lhs def in
    let rhs = serialize_exp @@ Def.rhs def in
    let dict = Dict.filter ~f:Tags.is_vibes_attr @@ Term.attrs def in
    let+ () = update_attrs tid dict in
    Sexp.List [Atom tid; Atom "set"; lhs; rhs]

  let serialize_label : label -> Sexp.t = function
    | Direct tid -> List [
        Atom "direct";
        Atom (Tid.to_string tid);
      ]
    | Indirect exp -> List [
        Atom "indirect";
        serialize_exp exp;
      ]

  let serialize_jmp (jmp : jmp term) : Sexp.t t =
    let tid = Tid.to_string (Term.tid jmp) in
    let cond = serialize_exp @@ Jmp.cond jmp in
    let dict = Dict.filter ~f:Tags.is_vibes_attr @@ Term.attrs jmp in
    let+ () = update_attrs tid dict in
    match Jmp.kind jmp with
    | Call c -> Sexp.List [
        Atom tid;
        Atom "call";
        serialize_label @@ Call.target c;
        List (
          let open Sexp in
          Call.return c |>
          Option.value_map ~default:[Atom "noreturn"] ~f:(fun l ->
              [Atom "return"; serialize_label l])
        );
        List [Atom "when"; cond];
      ]
    | Goto label -> Sexp.List [
        Atom tid;
        Atom "goto";
        serialize_label label;
        List [Atom "when"; cond];
      ]
    | Ret label -> Sexp.List [
        Atom tid;
        Atom "return";
        serialize_label label;
        List [Atom "when"; cond];
      ]
    | Int (n, t) -> Sexp.List [
        Atom tid;
        Atom "interrupt";
        Atom (Int.to_string n);
        Atom (Tid.to_string t);
        List [Atom "when"; cond];
      ]

  let serialize_phi (phi : phi term) : Sexp.t = List [
      Atom (Tid.to_string @@ Term.tid phi);
      Atom "phi";
      serialize_var @@ Phi.lhs phi;
      List (
        let open Sexp in
        Phi.values phi |> Seq.map ~f:(fun (tid, exp) -> List [
            Atom (Tid.to_string tid);
            serialize_exp exp;
          ]) |> Seq.to_list
      )
    ]

  let serialize_subterms
      (cls : ('a, 'b) cls)
      (t : 'a term)
      ~(f : 'b term -> Sexp.t t) : Sexp.t list t =
    Term.enum cls t |> M.Seq.map ~f >>| Seq.to_list

  let serialize_blk (blk : blk term) : Sexp.t t =
    let tid = Tid.to_string @@ Term.tid blk in
    let serialize_phi phi = !!(serialize_phi phi) in
    let* phi = serialize_subterms phi_t blk ~f:serialize_phi in
    let* data = serialize_subterms def_t blk ~f:serialize_def in
    let* ctrl = serialize_subterms jmp_t blk ~f:serialize_jmp in
    let dict = Dict.filter ~f:Tags.is_vibes_attr @@ Term.attrs blk in
    let+ () = update_attrs tid dict in
    Sexp.List [
      Atom tid;
      Atom "block";
      List [Atom "phi"; List phi];
      List [Atom "data"; List data];
      List [Atom "ctrl"; List ctrl];
    ]

  let serialize_sub (sub : sub term) : Sexp.t t =
    let name = Sub.name sub in
    let* blks = serialize_subterms blk_t sub ~f:serialize_blk in
    let dict = Dict.filter ~f:Tags.is_vibes_attr @@ Term.attrs sub in
    let+ () = update_attrs name dict in
    Sexp.List [Atom name; List blks]

  let serialize_attrs (env : Env.t) : Sexp.t =
    let sexp =
      Map.to_alist env.attrs |>
      List.filter_map ~f:(fun (s, dict) ->
          if Dict.is_empty dict then None
          else Some (Sexp.List [Atom s; Dict.sexp_of_t dict])) in
    List [Atom "attrs"; List sexp]

end

module Deserializer = struct

  module Env = struct

    (* Since we rely on tid and var string representations, we use
       this environment to track them as they are created, and then
       to make sure that their uses are consistent across the program. *)
    type t = {
      vars : var String.Map.t;
      tids : tid String.Map.t;
      attrs : dict String.Map.t;
    }

    let empty : t = {
      vars = String.Map.empty;
      tids = String.Map.empty;
      attrs = String.Map.empty;
    }

  end

  include Monad.State.T1(Env)(KB)
  include Monad.State.Make(Env)(KB)

  let fail (err : KB.conflict) : 'a t = lift @@ KB.fail err

  let lookup_var (s : string) : var option t =
    gets @@ fun {vars; _} -> Map.find vars s

  let lookup_tid (s : string) : tid option t =
    gets @@ fun {tids; _} -> Map.find tids s

  let add_var (s : string) (v : var) : unit t =
    update @@ fun env -> {
      env with vars = Map.set env.vars ~key:s ~data:v;
    }

  let add_attrs (s : string) (dict : dict) : unit t =
    update @@ fun env -> {
      env with attrs = Map.set env.attrs ~key:s ~data:dict;
    }

  let set_attrs (s : string) (t : 'a term) : 'a term t =
    gets @@ fun env -> match Map.find env.attrs s with
    | Some d -> Term.with_attrs t d
    | None -> t

  (* We can use this property to relate names in the [Function_info]
     structures with actual tids in the Knowledge Base. *)
  let provide_alias (s : string) (t : tid) : unit t =
    let alias = Set.singleton (module String) s in
    lift @@ KB.provide T.Label.aliases t alias

  let add_tid (s : string) (t : tid) : unit t =
    let* () = provide_alias s t in
    update @@ fun env -> {
      env with tids = Map.set env.tids ~key:s ~data:t;
    }

  let deserialize_tid (s : string) : tid t =
    let* tid = lookup_tid s in
    match tid with
    | Some tid -> !!tid
    | None ->
      let* tid = lift @@ T.Label.fresh in
      let+ () = add_tid s tid in
      tid

  let deserialize_int (s : string) : int t =
    match int_of_string_opt s with
    | Some i -> !!i
    | None ->
      let msg = Format.sprintf "Expected int, but got '%s'" s in
      fail @@ Errors.Invalid_bir msg

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
          fail @@ Errors.Invalid_bir msg
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected integer size, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_size : Sexp.t -> size t = function
    | Atom raw as sexp -> begin
        let* i = deserialize_int raw in
        match Size.of_int_opt i with
        | Some size -> !!size
        | None ->
          let msg = Format.asprintf
              "Expected Bap.Std.size, but got '%a'"
              Sexp.pp sexp in
          fail @@ Errors.Invalid_bir msg
      end
    | sexp ->
      let msg = Format.asprintf
          "Expected integer size, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_bool (s : string) : bool t =
    match bool_of_string_opt s with
    | Some i -> !!i
    | None ->
      let msg = Format.sprintf "Expected bool, but got '%s'" s in
      fail @@ Errors.Invalid_bir msg

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
      fail @@ Errors.Invalid_bir msg

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
          fail @@ Errors.Invalid_bir msg
        | Some v when not @@ same_virt is_virtual v ->
          let s b = if b then "virtual" else "physical" in
          let msg = Format.asprintf
              "Expected var '%s' to be %s, but got %s"
              name (s @@ Var.is_virtual v) (s is_virtual) in
          fail @@ Errors.Invalid_bir msg
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
      fail @@ Errors.Invalid_bir msg

  let deserialize_word (sexp : Sexp.t) : word t =
    let msg = Format.asprintf
        "Expected 'Bap.Std.word', but got: '%a'"
        Sexp.pp sexp in
    match sexp with
    | Atom w -> begin
        try !!(Word.of_string w)
        with _ -> fail @@ Errors.Invalid_bir msg
      end
    | _ -> fail @@ Errors.Invalid_bir msg

  let deserialize_endianness : Sexp.t -> endian t = function
    | Atom "BigEndian" -> !!BigEndian
    | Atom "LittleEndian" -> !!LittleEndian
    | sexp ->
      let msg =
        Format.asprintf
          "Expected 'BigEndian' or 'LittleEndian' endianness, \
           but got: '%a'" Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_cast : Sexp.t -> cast t = function
    | Atom "UNSIGNED" -> !!Bil.UNSIGNED
    | Atom "SIGNED" -> !!Bil.SIGNED
    | Atom "HIGH" -> !!Bil.HIGH
    | Atom "LOW" -> !!Bil.LOW
    | sexp ->
      let msg = Format.asprintf
          "Expected valid Bil.cast, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_unop : Sexp.t -> unop t = function
    | Atom "NEG" -> !!Bil.NEG
    | Atom "NOT" -> !!Bil.NOT
    | sexp ->
      let msg = Format.asprintf
          "Expected valid Bil.unop, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_binop : Sexp.t -> binop t = function
    | Atom "PLUS" -> !!Bil.PLUS
    | Atom "MINUS" -> !!Bil.MINUS
    | Atom "TIMES" -> !!Bil.TIMES
    | Atom "DIVIDE" -> !!Bil.DIVIDE
    | Atom "SDIVIDE" -> !!Bil.SDIVIDE
    | Atom "MOD" -> !!Bil.MOD
    | Atom "SMOD" -> !!Bil.SMOD
    | Atom "LSHIFT" -> !!Bil.LSHIFT
    | Atom "RSHIFT" -> !!Bil.RSHIFT
    | Atom "ARSHIFT" -> !!Bil.ARSHIFT
    | Atom "AND" -> !!Bil.AND
    | Atom "OR" -> !!Bil.OR
    | Atom "XOR" -> !!Bil.XOR
    | Atom "EQ" -> !!Bil.EQ
    | Atom "NEQ" -> !!Bil.NEQ
    | Atom "LT" -> !!Bil.LT
    | Atom "LE" -> !!Bil.LE
    | Atom "SLT" -> !!Bil.SLT
    | Atom "SLE" -> !!Bil.SLE
    | sexp ->
      let msg = Format.asprintf
          "Expected valid Bil.binop, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let rec deserialize_exp : Sexp.t -> exp t = function
    | List [Atom "load"; raw_e1; raw_e2; raw_endianness; raw_size] ->
      let* e1 = deserialize_exp raw_e1 in
      let* e2 = deserialize_exp raw_e2 in
      let* endianness = deserialize_endianness raw_endianness in
      let+ size = deserialize_size raw_size in
      Bil.Load (e1, e2, endianness, size)
    | List [Atom "store"; raw_e1; raw_e2; raw_e3; raw_endianness; raw_size] ->
      let* e1 = deserialize_exp raw_e1 in
      let* e2 = deserialize_exp raw_e2 in
      let* e3 = deserialize_exp raw_e3 in
      let* endianness = deserialize_endianness raw_endianness in
      let+ size = deserialize_size raw_size in
      Bil.Store (e1, e2, e3, endianness, size)
    | List [Atom "binop"; raw_binop; raw_e1; raw_e2] ->
      let* binop = deserialize_binop raw_binop in
      let* e1 = deserialize_exp raw_e1 in
      let+ e2 = deserialize_exp raw_e2 in
      Bil.BinOp (binop, e1, e2)
    | List [Atom "unop"; raw_unop; raw_e] ->
      let* unop = deserialize_unop raw_unop in
      let+ e = deserialize_exp raw_e in
      Bil.UnOp (unop, e)
    | List [Atom "var"; v] ->
      let+ v = deserialize_var v in
      Bil.Var v
    | List [Atom "int"; w] ->
      let+ w = deserialize_word w in
      Bil.Int w
    | List [Atom "cast"; raw_cast; Atom raw_i; raw_e] ->
      let* cast = deserialize_cast raw_cast in
      let* i = deserialize_int raw_i in
      let+ e = deserialize_exp raw_e in
      Bil.Cast (cast, i, e)
    | Sexp.List [Atom "ite"; raw_e1; raw_e2; raw_e3] ->
      let* e1 = deserialize_exp raw_e1 in
      let* e2 = deserialize_exp raw_e2 in
      let+ e3 = deserialize_exp raw_e3 in
      Bil.Ite (e1, e2, e3)
    | List [Atom "extract"; Atom raw_i1; Atom raw_i2; raw_e] ->
      let* i1 = deserialize_int raw_i1 in
      let* i2 = deserialize_int raw_i2 in
      let+ e = deserialize_exp raw_e in
      Bil.Extract (i1, i2, e)
    | List [Atom "concat"; raw_e1; raw_e2] ->
      let* e1 = deserialize_exp raw_e1 in
      let+ e2 = deserialize_exp raw_e2 in
      Bil.Concat (e1, e2)
    | List [Atom "unknown"; Atom s; t] ->
      let+ t = deserialize_typ t in
      Bil.Unknown (s, t)
    | List [Atom "let"; v; x; y] ->
      let* v = deserialize_var v in
      let* x = deserialize_exp x in
      let+ y = deserialize_exp y in
      Bil.Let (v, x, y)
    | sexp ->
      let msg = Format.asprintf
          "Can't deserialize expr: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_label : Sexp.t -> label t = function
    | List [Atom "direct"; Atom raw_tid] ->
      let+ tid = deserialize_tid raw_tid in
      Direct tid
    | List [Atom "indirect"; raw_e] ->
      let+ e = deserialize_exp raw_e in
      Indirect e
    | sexp ->
      let msg = Format.asprintf
          "Expected 'direct tid' or 'indirect exp' but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_return : Sexp.t -> label option t = function
    | List [Atom "return"; raw_dst] ->
      let+ dst = deserialize_label raw_dst in
      Some dst
    | List [Atom "noreturn"] -> !!None
    | sexp ->
      let msg = Format.asprintf
          "Expected 'return label' or 'noreturn' but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_phi : Sexp.t -> phi term t = function
    | List [Atom raw_tid; Atom "phi"; raw_lhs; List raw_values] ->
      let* tid = deserialize_tid raw_tid in
      let* lhs = deserialize_var raw_lhs in
      let+ values = List.map raw_values ~f:(function
          | List [Atom raw_tid; raw_exp] ->
            let* tid = deserialize_tid raw_tid in
            let+ exp = deserialize_exp raw_exp in
            tid, exp
          | sexp ->
            let msg = Format.asprintf
                "Expected (tid exp) for phi, but got: '%a'"
                Sexp.pp sexp in
            fail @@ Errors.Invalid_bir msg) in
      Phi.of_list lhs values ~tid
    | sexp ->
      let msg = Format.asprintf "Expected phi, but got: '%a'" Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_attr : Sexp.t -> unit t = function
    | List [Atom tid; dict] -> begin
        try add_attrs tid @@ Dict.t_of_sexp dict with
        | exn ->
          let msg = Format.asprintf
              "Failed to parse attrs '%a' for tid %s: %a"
              Sexp.pp dict tid Core.Exn.pp exn in
          fail @@ Errors.Invalid_bir msg
      end
    | sexp ->
      let msg = Format.asprintf "Expected attr, but got: '%a'" Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_attrs : Sexp.t -> unit t = function
    | List [Atom "attrs"; List attrs] -> List.iter attrs ~f:deserialize_attr
    | sexp ->
      let msg = Format.asprintf "Expected attrs, but got: '%a'" Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_def : Sexp.t -> def term t = function
    | List [Atom raw_tid; Atom "set"; raw_lhs; raw_rhs] ->
      let* tid = deserialize_tid raw_tid in
      let* v = deserialize_var raw_lhs in
      let* exp = deserialize_exp raw_rhs in
      set_attrs raw_tid @@ Def.create v exp ~tid
    | sexp ->
      let msg = Format.asprintf "Expected def, but got: '%a'" Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_jmp : Sexp.t -> jmp term t = function
    | List [
        Atom raw_tid;
        Atom "call";
        raw_dst;
        raw_return;
        List [Atom "when"; raw_exp]
      ] ->
      let* tid = deserialize_tid raw_tid in
      let* target = deserialize_label raw_dst in
      let* return = deserialize_return raw_return in
      let* cond = deserialize_exp raw_exp  in
      let call = Call.create () ~target ?return in
      set_attrs raw_tid @@ Jmp.create_call call ~tid ~cond
    | List [
        Atom raw_tid;
        Atom "goto";
        raw_dst;
        List [Atom "when"; raw_exp]
      ] ->
      let* tid = deserialize_tid raw_tid in
      let* dst = deserialize_label raw_dst in
      let* cond = deserialize_exp raw_exp in
      set_attrs raw_tid @@ Jmp.create_goto dst ~tid ~cond
    | List [
        Atom raw_tid;
        Atom "return";
        raw_dst;
        List [Atom "when"; raw_exp]
      ] ->
      let* tid = deserialize_tid raw_tid in
      let* dst = deserialize_label raw_dst in
      let* cond = deserialize_exp raw_exp in
      set_attrs raw_tid @@ Jmp.create_ret dst ~tid ~cond
    | List [
        Atom raw_tid;
        Atom "interrupt";
        Atom n;
        Atom t;
        List [Atom "when"; raw_exp];
      ] ->
      let* tid = deserialize_tid raw_tid in
      let* n = deserialize_int n in
      let* t = deserialize_tid t in
      let* cond = deserialize_exp raw_exp in
      set_attrs raw_tid @@ Jmp.create_int n t ~tid ~cond
    | sexp ->
      let msg = Format.asprintf
          "Expected call, goto, or ret jmp but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_blk : Sexp.t -> blk term t = function
    | List [
        Atom raw_tid;
        Atom "block";
        List [Atom "phi"; List raw_phis];
        List [Atom "data"; List raw_defs];
        List [Atom "ctrl"; List raw_jmps];
      ] ->
      let* tid = deserialize_tid raw_tid in
      let* phis = List.map raw_phis ~f:deserialize_phi in
      let* defs = List.map raw_defs ~f:deserialize_def in
      let* jmps = List.map raw_jmps ~f:deserialize_jmp in
      set_attrs raw_tid @@ Blk.create () ~tid ~phis ~defs ~jmps
    | sexp ->
      let msg = Format.asprintf
          "Expected block, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

  let deserialize_blks : Sexp.t list -> blk term list t =
    List.map ~f:deserialize_blk

  let deserialize_sub : Sexp.t -> sub term t = function
    | List [Atom name; List blks] ->
      let* blks = deserialize_blks blks in
      let* sub = lift @@ Helpers.create_sub name blks in
      set_attrs name sub
    | sexp ->
      let msg = Format.asprintf
          "Expected sub, but got: '%a'"
          Sexp.pp sexp in
      fail @@ Errors.Invalid_bir msg

end

let serialize_var (v : var) : Sexp.t =
  Serializer.serialize_var v

let serialize (sub : sub term) : Sexp.t =
  let open Serializer in
  let sub, env = M.run (serialize_sub sub) Env.empty in
  List [Atom "program"; sub; serialize_attrs env]

let deserialize : Sexp.t -> sub term KB.t = function
  | List [Atom "program"; sub; attrs] ->
    let open Deserializer in
    let open KB.Syntax in
    let* (), env = run (deserialize_attrs attrs) Env.empty in
    let+ sub, _  = run (deserialize_sub   sub)   env in
    sub
  | sexp -> KB.fail @@ Errors.Invalid_bir (
      Format.asprintf "Expected program, but got: '%a'"
        Sexp.pp sexp)
