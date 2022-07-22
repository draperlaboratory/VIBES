open Core
open Bap.Std
open Vibes_error_lib.Let

module T = Bap_core_theory.Theory
module Err = Vibes_error_lib.Std

module Serializer = struct

  let serialize_size (sz : size) : Sexp.t =
    let bits = Size.in_bits sz in
    Sexp.Atom (Format.sprintf "%d" bits)

  let serialize_endianness (e : endian) : Sexp.t =
    match e with
    | LittleEndian -> Sexp.Atom "LittleEndian"
    | BigEndian -> Sexp.Atom "BigEndian"

  let serialize_cast (c : cast) : Sexp.t =
    match c with
    | UNSIGNED -> Sexp.Atom "UNSIGNED"
    | SIGNED -> Sexp.Atom "SIGNED"
    | HIGH -> Sexp.Atom "HIGH"
    | LOW -> Sexp.Atom "LOW"

  let serialize_unop (op : unop) : Sexp.t =
    match op with
    | NEG -> Sexp.Atom "NEG"
    | NOT -> Sexp.Atom "NOT"

  let serialize_binop (op : binop) : Sexp.t =
    match op with
    | PLUS -> Sexp.Atom "PLUS"
    | MINUS -> Sexp.Atom "MINUS"
    | TIMES -> Sexp.Atom "TIMES"
    | DIVIDE -> Sexp.Atom "DIVIDE"
    | SDIVIDE -> Sexp.Atom "SDIVIDE"
    | MOD -> Sexp.Atom "MOD"
    | SMOD -> Sexp.Atom "SMOD"
    | LSHIFT -> Sexp.Atom "LSHIFT"
    | RSHIFT -> Sexp.Atom "RSHIFT"
    | ARSHIFT -> Sexp.Atom "ARSHIFT"
    | AND -> Sexp.Atom "AND"
    | OR -> Sexp.Atom "OR"
    | XOR -> Sexp.Atom "XOR"
    | EQ -> Sexp.Atom "EQ"
    | NEQ -> Sexp.Atom "NEQ"
    | LT -> Sexp.Atom "LT"
    | LE -> Sexp.Atom "LE"
    | SLT -> Sexp.Atom "SLT"
    | SLE -> Sexp.Atom "SLE"

  let rec serialize_exp (exp : Exp.t) : (Sexp.t, Err.t) result =
    match exp with
    | Load (e1, e2, endianness, size) ->
       let- e1' = serialize_exp e1 in
       let- e2' = serialize_exp e2 in
       let endianness' = serialize_endianness endianness in
       let size' = serialize_size size in
       Ok (Sexp.List [Atom "load"; e1'; e2'; endianness'; size'])
    | Store (e1, e2, e3, endianness, size) ->
       let- e1' = serialize_exp e1 in
       let- e2' = serialize_exp e2 in
       let- e3' = serialize_exp e3 in
       let endianness' = serialize_endianness endianness in
       let size' = serialize_size size in
       Ok (Sexp.List [Atom "store"; e1'; e2'; e3'; endianness'; size'])
    | BinOp (op, e1, e2) ->
       let- e1' = serialize_exp e1 in
       let- e2' = serialize_exp e2 in
       let op' = serialize_binop op in
       Ok (Sexp.List [Atom "binop"; op'; e1'; e2'])
    | UnOp (op, e) ->
       let- e' = serialize_exp e in
       let op' = serialize_unop op in
       Ok (Sexp.List [Atom "unop"; op'; e'])
    | Var v ->
       Ok (Sexp.List [Atom "var"; Atom (Var.name v)])
    | Int w ->
       Ok (Sexp.List [Atom "int"; Atom (Word.to_string w)])
    | Cast (cast, i, e) ->
       let- e' = serialize_exp e in
       let cast' = serialize_cast cast in
       let i' = Sexp.Atom (Format.sprintf "%d" i) in
       Ok (Sexp.List [Atom "cast"; cast'; i'; e'])
    | Ite (e1, e2, e3) ->
       let- e1' = serialize_exp e1 in
       let- e2' = serialize_exp e2 in
       let- e3' = serialize_exp e3 in
       Ok (Sexp.List [Atom "ite"; e1'; e2'; e3'])
    | Extract (i1, i2, e) ->
       let- e' = serialize_exp e in
       let i1' = Sexp.Atom (Format.sprintf "%d" i1) in
       let i2' = Sexp.Atom (Format.sprintf "%d" i2) in
       Ok (Sexp.List [Atom "extract"; i1'; i2'; e'])
    | Concat (e1, e2) ->
       let- e1' = serialize_exp e1 in
       let- e2' = serialize_exp e2 in
       Ok (Sexp.List [Atom "concat"; e1'; e2'])
    | _ ->
       let msg =
         Format.sprintf "Can't serialize exp: '%s'" (Exp.to_string exp)
       in
       Error (Types.Unhandled_bir msg)

  let serialize_def (def : Def.t) : (Sexp.t, Err.t) result =
    let tid = Tid.to_string (Term.tid def) in
    let lhs = Def.lhs def in
    let rhs = Def.rhs def in
    let var = Var.name lhs in
    let- exp = serialize_exp rhs in
    Ok (Sexp.List [Atom tid; Atom "set"; List [Atom "var"; Atom var]; exp])

  let serialize_label (l : label) : (Sexp.t, Err.t) result =
    match l with
    | Direct tid ->
       Ok (Sexp.List [Atom "direct"; Atom (Tid.to_string tid)])
    | Indirect exp ->
       let- exp' = serialize_exp exp in
       Ok (Sexp.List [Atom "indirect"; exp'])

  let serialize_jmp (jmp : Jmp.t) : (Sexp.t, Err.t) result =
    let tid = Tid.to_string (Term.tid jmp) in
    let cond = Jmp.cond jmp in
    let- exp = serialize_exp cond in
    match Jmp.kind jmp with
    | Call c ->
       let- dst = serialize_label (Call.target c) in
       let- return = match Call.return c with
         | Some l ->
            let- l' = serialize_label l in
            Ok (Sexp.List [Atom "return"; l'])
         | None -> Ok (Sexp.List [Atom "no-return"])
       in
       let result = Sexp.List [Atom tid; Atom "call"; dst; return;
         List [Atom "when"; exp]]
       in
       Ok result
    | Goto label ->
       let- dst = serialize_label label in
       Ok (Sexp.List [Atom tid; Atom "goto"; dst; List [Atom "when"; exp]])
    | Ret label ->
       let- dst = serialize_label label in
       Ok (Sexp.List [Atom tid; Atom "return"; dst; List [Atom "when"; exp]])
    | Int (_, _) ->
       let msg = Format.sprintf
         "Can't serialize jmp: '%s'"
         (Jmp.to_string jmp)
       in
       Error (Types.Unhandled_bir msg)

  let serialize_blk (blk : Blk.t) : (Sexp.t, Err.t) result =
    let tid = Tid.to_string (Term.tid blk) in
    let orig_phis = Seq.to_list (Term.enum phi_t blk) in
    let- _ =
      if List.length orig_phis > 0 then
        let msg =
          Format.sprintf
            "We don't serialize phi nodes in blk '%s'"
            (Blk.to_string blk)
        in
        Error (Types.Unhandled_bir msg)
      else Ok ()
    in
    let orig_defs = Seq.to_list (Term.enum def_t blk) in
    let- defs = Result.all (List.map orig_defs ~f:serialize_def) in
    let orig_jmps = Seq.to_list (Term.enum jmp_t blk) in
    let- jmps = Result.all (List.map orig_jmps ~f:serialize_jmp) in
    Ok (Sexp.List [
      Atom tid;
      Atom "block";
      Sexp.List [Atom "data"; Sexp.List defs];
      Sexp.List [Atom "ctrl"; Sexp.List jmps];
    ])

end

module Deserializer = struct

  let s_of = Sexp.to_string

  let word_size (target : T.Target.t) : typ =
    let bits = T.Target.bits target in
    Imm bits

  let deserialize_tid (s : string) : (Tid.t, Err.t) result =
    match Tid.from_string s with
    | Ok tid -> Ok tid
    | Error e ->
       let e_str = Error.to_string_hum e in
       let msg = Format.sprintf "Error deserializing tid: '%s'" e_str in
       Error (Types.Invalid_bir msg)

  let deserialize_int (s : string) : (int, Err.t) result =
    match int_of_string_opt s with
    | Some i -> Ok i
    | None ->
       let msg = Format.sprintf "Expected int, but got '%s'" s in
       Error (Types.Invalid_bir msg)

  let deserialize_var ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (Var.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom "var"; Atom raw_v] ->
       let size = word_size target in
       let v = Var.create raw_v size in
       Ok v
    | _ ->
       let msg =
         Format.sprintf "Expected '(var x)', but got: '%s'" (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_word (sexp : Sexp.t) : (Word.t, Err.t) result =
    let msg = Format.sprintf
      "Expected '(int w)', but got: '%s'"
      (s_of sexp)
    in
    match sexp with
    | Sexp.List [Atom "int"; Atom w] ->
       begin
         try Ok (Word.of_string w)
         with _ -> Error (Types.Invalid_bir msg)
       end
    | _ -> Error (Types.Invalid_bir msg)

  let deserialize_endianness (sexp : Sexp.t) : (endian, Err.t) result =
    match sexp with
    | Sexp.Atom "BigEndian" -> Ok BigEndian
    | Sexp.Atom "LittleEndian" -> Ok LittleEndian
    | _ ->
       let msg =
         Format.sprintf
           "Expected 'BigEndian' or 'LittleEndian' endianness, but got: '%s'"
           (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_size (sexp : Sexp.t) : (size, Err.t) result =
    match sexp with
    | Sexp.Atom raw_i ->
       let- i = deserialize_int raw_i in
       begin
         match Size.of_int_opt i with
         | Some size -> Ok size
         | None ->
            let msg = Format.sprintf
              "Expected Bap.Std.size, but got '%s'"
              (s_of sexp)
            in
            Error (Types.Invalid_bir msg)
       end
    | _ ->
       let msg = Format.sprintf
         "Expected integer size, but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_cast (sexp : Sexp.t) : (cast, Err.t) result =
    match sexp with
    | Sexp.Atom "UNSIGNED" -> Ok UNSIGNED
    | Sexp.Atom "SIGNED" -> Ok SIGNED
    | Sexp.Atom "HIGH" -> Ok HIGH
    | Sexp.Atom "LOW" -> Ok LOW
    | _ ->
       let msg = Format.sprintf
         "Expected valid Bil.cast, but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_unop (sexp : Sexp.t) : (unop, Err.t) result =
    match sexp with
    | Sexp.Atom "NEG" -> Ok NEG
    | Sexp.Atom "NOT" -> Ok NOT
    | _ ->
       let msg = Format.sprintf
         "Expected valid Bil.unop, but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_binop (sexp : Sexp.t) : (binop, Err.t) result =
    match sexp with
    | Sexp.Atom "PLUS" -> Ok PLUS
    | Sexp.Atom "MINUS" -> Ok MINUS
    | Sexp.Atom "TIMES" -> Ok TIMES
    | Sexp.Atom "DIVIDE" -> Ok DIVIDE
    | Sexp.Atom "SDIVIDE" -> Ok SDIVIDE
    | Sexp.Atom "MOD" -> Ok MOD
    | Sexp.Atom "SMOD" -> Ok SMOD
    | Sexp.Atom "LSHIFT" -> Ok LSHIFT
    | Sexp.Atom "RSHIFT" -> Ok RSHIFT
    | Sexp.Atom "ARSHIFT" -> Ok ARSHIFT
    | Sexp.Atom "AND" -> Ok AND
    | Sexp.Atom "OR" -> Ok OR
    | Sexp.Atom "XOR" -> Ok XOR
    | Sexp.Atom "EQ" -> Ok EQ
    | Sexp.Atom "NEQ" -> Ok NEQ
    | Sexp.Atom "LT" -> Ok LT
    | Sexp.Atom "LE" -> Ok LE
    | Sexp.Atom "SLT" -> Ok SLT
    | Sexp.Atom "SLE" -> Ok SLE
    | _ ->
       let msg = Format.sprintf
         "Expected valid Bil.binop, but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let rec deserialize_exp ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (Exp.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom "load"; raw_e1; raw_e2; raw_endianness; raw_size] ->
       let- e1 = deserialize_exp raw_e1 ~target in
       let- e2 = deserialize_exp raw_e2 ~target in
       let- endianness = deserialize_endianness raw_endianness in
       let- size = deserialize_size raw_size in
       Ok (Bil.Load (e1, e2, endianness, size))
    | Sexp.List
         [Atom "store"; raw_e1; raw_e2; raw_e3; raw_endianness; raw_size] ->
       let- e1 = deserialize_exp raw_e1 ~target in
       let- e2 = deserialize_exp raw_e2 ~target in
       let- e3 = deserialize_exp raw_e3 ~target in
       let- endianness = deserialize_endianness raw_endianness in
       let- size = deserialize_size raw_size in
       Ok (Bil.Store (e1, e2, e3, endianness, size))
    | Sexp.List [Atom "binop"; raw_binop; raw_e1; raw_e2] ->
       let- binop = deserialize_binop raw_binop in
       let- e1 = deserialize_exp raw_e1 ~target in
       let- e2 = deserialize_exp raw_e2 ~target in
       Ok (Bil.BinOp (binop, e1, e2))
    | Sexp.List [Atom "unop"; raw_unop; raw_e] ->
       let- unop = deserialize_unop raw_unop in
       let- e = deserialize_exp raw_e ~target in
       Ok (Bil.UnOp (unop, e))
    | Sexp.List [Atom "var"; _] ->
       let- v = deserialize_var sexp ~target in
       Ok (Bil.Var v)
    | Sexp.List [Atom "int"; _] ->
       let- w = deserialize_word sexp in
       Ok (Bil.Int w)
    | Sexp.List [Atom "cast"; raw_cast; Atom raw_i; raw_e] ->
       let- cast = deserialize_cast raw_cast in
       let- i = deserialize_int raw_i in
       let- e = deserialize_exp raw_e ~target in
       Ok (Bil.Cast (cast, i, e))
    | Sexp.List [Atom "ite"; raw_e1; raw_e2; raw_e3] ->
       let- e1 = deserialize_exp raw_e1 ~target in
       let- e2 = deserialize_exp raw_e2 ~target in
       let- e3 = deserialize_exp raw_e3 ~target in
       Ok (Bil.Ite (e1, e2, e3))
    | Sexp.List [Atom "extract"; Atom raw_i1; Atom raw_i2; raw_e] ->
       let- i1 = deserialize_int raw_i1 in
       let- i2 = deserialize_int raw_i2 in
       let- e = deserialize_exp raw_e ~target in
       Ok (Bil.Extract (i1, i2, e))
    | Sexp.List [Atom "concat"; raw_e1; raw_e2] ->
       let- e1 = deserialize_exp raw_e1 ~target in
       let- e2 = deserialize_exp raw_e2 ~target in
       Ok (Bil.Concat (e1, e2))
    | _ ->
       let msg = Format.sprintf "Can't deserialize expr: '%s'" (s_of sexp) in
       Error (Types.Invalid_bir msg)

  let deserialize_label ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (label, Err.t) result =
    match sexp with
    | Sexp.List [Atom "direct"; Atom raw_tid] ->
       let- tid = deserialize_tid raw_tid in
       Ok (Direct tid)
    | Sexp.List [Atom "indirect"; raw_e] ->
       let- e = deserialize_exp raw_e ~target in
       Ok (Indirect e)
    | _ ->
       let msg = Format.sprintf
         "Expected 'direct tid' or 'indirect exp' but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_return ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (label option, Err.t) result =
    match sexp with
    | Sexp.List [Atom "return"; raw_dst] ->
       let- dst = deserialize_label raw_dst ~target in
       Ok (Some dst)
    | Sexp.List [Atom "no-return"] -> Ok None
    | _ ->
       let msg = Format.sprintf
         "Expected 'return label' or 'no-return' but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_def ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (Def.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom raw_tid; Atom "set"; raw_lhs; raw_rhs] ->
       let- tid = deserialize_tid raw_tid in
       let- v = deserialize_var raw_lhs ~target in
       let- exp = deserialize_exp raw_rhs ~target in
       let def = Def.create v exp ~tid in
       Ok def
    | _ ->
       let msg = Format.sprintf "Expected def, but got: '%s'" (s_of sexp) in
       Error (Types.Invalid_bir msg)

  let deserialize_jmp ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (Jmp.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom raw_tid; Atom "call"; raw_dst; raw_return;
         List [Atom "when"; raw_exp]] ->
       begin
         let- tid = deserialize_tid raw_tid in
         let- dst = deserialize_label raw_dst ~target in
         let- ret = deserialize_return raw_return ~target in
         let- cond = deserialize_exp raw_exp ~target in
         let call_prototype = Call.create () ~target:dst in
         let call = match ret with
           | Some label -> Call.with_return call_prototype label
           | None -> Call.with_noreturn call_prototype
         in
         let jmp = Jmp.create_call call ~tid ~cond in
         Ok jmp
       end
    | Sexp.List [Atom raw_tid; Atom "goto"; raw_dst;
         List [Atom "when"; raw_exp]] ->
       let- tid = deserialize_tid raw_tid in
       let- dst = deserialize_label raw_dst ~target in
       let- cond = deserialize_exp raw_exp ~target in
       let jmp = Jmp.create_goto dst ~tid ~cond in
       Ok jmp
    | Sexp.List [Atom raw_tid; Atom "return"; raw_dst;
         List [Atom "when"; raw_exp]] ->
       let- tid = deserialize_tid raw_tid in
       let- dst = deserialize_label raw_dst ~target in
       let- cond = deserialize_exp raw_exp ~target in
       let jmp = Jmp.create_ret dst ~tid ~cond in
       Ok jmp
    | _ ->
       let msg = Format.sprintf
         "Expected call, goto, or ret jmp but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

  let deserialize_blk ~target:(target : T.Target.t) (sexp : Sexp.t)
      : (Blk.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom raw_tid; Atom "block"; List [Atom "data"; List raw_defs];
         List [Atom "ctrl"; Sexp.List raw_jmps]] ->
       let- tid = deserialize_tid raw_tid in
       let- defs = Result.all
         (List.map raw_defs ~f:(fun def -> deserialize_def def ~target))
       in
       let- jmps = Result.all
         (List.map raw_jmps ~f:(fun jmp -> deserialize_jmp jmp ~target))
       in
       let blk = Blk.create () ~tid ~defs ~jmps in
       Ok blk
    | _ ->
       let msg = Format.sprintf
         "Expected block, but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_bir msg)

end

let serialize = Serializer.serialize_blk
let deserialize = Deserializer.deserialize_blk
