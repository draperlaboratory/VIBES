open Core_kernel
open Cabs

(* Parses a string into a Cabs.file *)
let parse_C_file (input : string) : (Cabs.file, string) result =
  let open Clexer in
  try
    init_lexicon ();
    Ok (Cparser.file initial (Lexing.from_string input))
  with
    (* | Cparser.Error -> Error "Cparser.Error" *)
    | Parsing.Parse_error -> Error "Error"
    | Cabs.BadType -> Error "BadType"
    | Cabs.BadModifier -> Error "BadModifier"
    | Invalid_argument s -> Error s

(* Parses a sequence of C statements (a body) by wrapping it in a dummy function, parsing that as a file
   and then extracting the body from the parsed dummy function *)
let parse_C_body (b : string) : (Cabs.definition list * Cabs.statement, string) result  =
  let (let*) = Result.(>>=) in
  let p = Printf.sprintf "int FRONTC_PARSING_DUMMY_FUNCTION(){\n%s\n}" b in
  let* parse_result = parse_C_file p in
  match parse_result with
    | [FUNDEF (_, (defs, stmt))] -> Ok (defs, stmt)
    | _ -> Error "VIBES: Unexpected Form in FrontC Parse"


let binop_to_string (op : Cabs.binary_operator) : string =
  match op with
  | ADD -> "+"   (* "+" operator. *)
  | SUB -> "-"   (* "-" operator. *)
  | MUL -> "*"   (* "*" operator. *)
  | DIV -> "/"   (* "/" operator. *)
  | ASSIGN -> "set"  (* "=" operator. *)
  | SHL -> "<<"   (* "<<" operator. *)
  | SHR  -> ">>"  (* ">>" operator. *)
  | EQ  -> "=="  (* "==" operator. *)
  | NE  -> "!="  (* "!=" operator. *)
  | LT  -> "<"  (* "<" operator. *)
  | GT  -> ">"  (* ">" operator. *)
  | LE  -> "<="  (* "<=" operator. *)
  | GE  -> ">="  (* ">=" operator. *)
  | AND   (* "&&" operator. *)
  | OR    (* "||" operator. *)
  | MOD   (* "%" operator. *)
  | BAND    (* "&" operator. *)
  | BOR    (* "|" operator. *)
  | XOR    (* "^" operator. *)
  | ADD_ASSIGN  (* "+=" operator. *)
  | SUB_ASSIGN  (* "-=" operator. *)
  | MUL_ASSIGN  (* "*=" operator. *)
  | DIV_ASSIGN  (* "/=" operator. *)
  | MOD_ASSIGN  (* "%=" operator. *)
  | BAND_ASSIGN  (* "&=" operator. *)
  | BOR_ASSIGN  (* "|=" operator. *)
  | XOR_ASSIGN  (* "^=" operator. *)
  | SHL_ASSIGN  (* "<<=" operator. *)
  | SHR_ASSIGN  (* ">>=" operator. *) -> failwith "FrontC produced binary operator unsupported by VIBES"

  let unop_to_string (op : Cabs.unary_operator) : string =
    (* Unary operators identifiers. *)
   match op with
  | MEMOF  (* "*" operator. *) -> "load"
  | MINUS  (* "-" operator. *)
  | PLUS  (* "+" operator. *)
  | NOT  (* "!" operator. *)
  | BNOT  (* "~" operator. *)
  | ADDROF (* "&" operator. *)
  | PREINCR (* "++" pre-incrementation. *)
  | PREDECR (* "--" pre-decrementation. *)
  | POSINCR (* "++" post-incrementation. *)
  | POSDECR (* "--" post-decrementation. *) -> failwith "FrontC produced unary operator unsupported by VIBES"

let constant_to_string (c :Cabs.constant) : string =
  match c with
  | CONST_INT s -> s
  | CONST_FLOAT s -> s
  | CONST_CHAR s -> s
  | CONST_STRING s -> s
  | CONST_COMPOUND _ -> failwith "FrontC produced compound constant unsupported by VIBES"

let rec expr_to_string (e : Cabs.expression) : string =
  match e with
    | UNARY (op, a) -> Printf.sprintf "(%s %s)" (unop_to_string op) (expr_to_string a)
    | BINARY (op, a, b) -> Printf.sprintf "(%s %s %s)"
                          (binop_to_string op) (expr_to_string a) (expr_to_string b)
    | INDEX (a, i) -> Printf.sprintf "(load (+ %s %s))"
                      (expr_to_string a) (expr_to_string i)
    | VARIABLE x -> x
    | CONSTANT c -> constant_to_string c
    | _ -> Cprint.print_expression e 0;
          failwith "FrontC produced expression unsupported by VIBES"

let rec stmt_to_string (s : Cabs.statement) : string =
  match s with
  | NOP -> ""
  | COMPUTATION e -> expr_to_string e
  | IF (c, BLOCK([], (GOTO _ as t)), BLOCK([], (GOTO _ as e))) -> Printf.sprintf "(branch %s %s %s)"
      (expr_to_string c) (stmt_to_string t) (stmt_to_string e)
  | IF _ -> failwith "Only if statements of form `if(c){goto l1;}else{goto l2;};` supported"
  | SEQUENCE (s1,s2) -> Printf.sprintf "%s\n%s" (stmt_to_string s1) (stmt_to_string s2)
  | GOTO "fallthrough" -> "fallthrough"
  | GOTO label -> Printf.sprintf "(goto %s)" label
  | BLOCK ([],s) -> stmt_to_string s 
      (* TODO: Probably not right. Deal with variable declarations. Do blocks has sexp syntax? *)
  | _ -> Cprint.print_statement s;
         failwith "FrontC produced statement unsupported by VIBES"

let def_to_string (def : Cabs.definition) : string =
  match def with
    | DECDEF (_typ,_storage,names) -> String.concat ~sep:" " (List.map ~f:(fun (n,_,_,_) -> n) names)
    | _ -> failwith "Expected DECDEF"

let defs_to_string (defs : Cabs.definition list) : string =
  let decls = List.map ~f:def_to_string defs in
  let decl_str = String.concat ~sep:" " decls in
  Printf.sprintf "(var-decls %s)" decl_str


let body_to_string ((defs,stmt) : Cabs.body) : string =
  Printf.sprintf "%s\n%s" (defs_to_string defs) (stmt_to_string stmt)

let c_patch_to_sexp_string (patch_str : string) : (string, string) result =
  let (let*) = Result.(>>=) in
  let* body = parse_C_body patch_str in
  Ok (body_to_string body)