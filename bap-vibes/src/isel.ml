open Core_kernel
open Bap.Std
open Bap_knowledge
module KB = Knowledge
(*-------------------------------------------------------

It is assumed that the instruction selector is receiving an SSA and flattened BIR
data structure.

Flattening the BIR (making the right hand side of a Def a non resursive expression)
simplifies significantly the problem and brings the data struture in correspondence
with the nodes of the Blindell universal instruction selection paper.
Def.t become identified with Blindell's computation nodes.

*)

module Pattern = struct
(** Patterns are a list of [Blk.t]. Variables inside of patterns are patterns variables
   which will be instantiated
*)
type t = Blk.t list
type pat = t


(** [blk_match] holds a matchee field Blk.t [mblk] and a pattern Blk.t [pblk] *)
type blk_match = {
  pblk : Blk.t;
  mblk : Blk.t
} [@@deriving equal, sexp, compare, hash]

(** [def_match] holds a matchee field Def.t [mblk] and a pattern Def.t [pblk] *)
type def_match = {
  pdef : Def.t;
  mdef : Def.t
} [@@deriving equal, sexp, compare, hash]

(** [jmp_match] holds a matchee field Jmp.t [mjmp] and a pattern Jmp.t [pjmp] *)
type jmp_match = {
  pjmp : Jmp.t;
  mjmp : Jmp.t
} [@@deriving equal, sexp, compare, hash]

(**
 [match_] holds a mapping from pattern [Var.t] and [Tid.t] to matchee [Exp.t] and [Tid.t] respectively.
 Variables from the pattern may match literals. Note that pattern Var.t match to Exp.t, which may be 
*)
type match_ = {
  vmap : Exp.t Var.Map.t;
  blk_map : blk_match Tid.Map.t;
  def_map : def_match Tid.Map.t;
  jmp_map : jmp_match Tid.Map.t
  (* TODO
  phi_map : phi_match Tid.Map.t;
  *)
} [@@deriving sexp]

let empty_match_ : match_ =
  {blk_map = Tid.Map.empty;
  def_map = Tid.Map.empty;
  vmap = Var.Map.empty;
  jmp_map = Tid.Map.empty}

(**
  Because we are matching over graph structures, when we check the validity of some node
  match, it may produce new obligations that other nodes must match.
  We do not immediately recurse to check these obligations but instead
  output them as obligations.
  In this way, already checked obligations can be memoized, avoiding an infinite loop.
  Obligations is related to the [match_] structure above, basically one constructor
  per field of the match_ record.
*)

module Obligations = struct
  type t = Var of Var.t * Exp.t | Def of def_match | Blk of blk_match | Jmp of jmp_match
end
module Ob = Obligations

(*
 [env] is required because when looking in Jmp.t we need to turn Tid.t back to Blk.t
 There may be other information that needs to go in [env] down the line
*)

type env = {
  blk_map : Blk.t Tid.Map.t
}

(**
  [check_exp] checks the compatbility of a matchee expressions and pattern expression.
  It lightly checks compatibility of expressions and leaves the remaining check as a 
  [Ob.Var] check
  It is a design choice to not make check_exp called in the obligations loop.
  Perhaps it should.

  *)
let check_exp ~mexp:(mexp : exp) ~pexp:(pexp : exp) : Ob.t list option =
    match pexp with
    | Var pv ->
              begin
              match mexp with
                | Var _ | Int _ -> Some ([Var (pv, mexp)])
                | _ -> failwith (sprintf "Isel.check_exp: Matchee not in Flattened Form %a" Exp.pps mexp)
              end
    | Int _i -> if (Exp.equal mexp pexp) then (Some []) else None
    | _ -> failwith (sprintf "Isel.check_exp: Pattern not in Flattened Form %a" Exp.pps pexp)


(**
  [check_def] given a candidate match between a pattern [Def.t] and matchee
  [Def.t], this will produce None if this candidate match is impossible or
  will produce `Some obligations` with remaining obligations / inferred match
  candidates that should also be checked.
*)
(*
TODO or at least be careful.
check_def and check_jmp should be checking that the block it belongs to is a block that makes sense.
This is fine *)
let check_def (mdef : Def.t) (pdef : Def.t) : Ob.t list option =
  let open Bil in
  let (let*) x f = Option.bind ~f x in
  let mlhs = Def.lhs mdef in
  let mrhs = Def.rhs mdef in
  let plhs = Def.lhs pdef in
  let prhs = Def.rhs pdef in
  (* The left hand side of Def.t must be matchable *)
  let obs = [Ob.Var (plhs, Var mlhs)] in
  let obs = match mrhs, prhs with

  | BinOp (binop, a, b), BinOp (binop', pa, pb) ->
      if Int.(compare_binop binop binop' = 0) then
        begin
        let* obsa = check_exp ~mexp:a ~pexp:pa in
        let* obsb = check_exp ~mexp:b ~pexp:pb in
        Some (obsa @ obsb @ obs)
        end
     else None
  | _, BinOp (_,_ ,_) -> None
  | BinOp (_,_ ,_), _ -> None

  | UnOp(unop, a), UnOp(unop', pa) ->
    if Int.(compare_unop unop unop' = 0)
      then let* obsa = check_exp ~mexp:a ~pexp:pa in Some (obsa @ obs)
      else None
  | _, UnOp(_,_) -> None
  | UnOp(_,_), _ -> None

  | Store (mem , addr, val_, endian, size),
    Store (pmem , paddr, pval_, pendian, psize)  ->
      if (Size.equal size psize && Int.(0 = compare_endian endian pendian)) then
        begin
          let* obs_mem = check_exp ~mexp:mem ~pexp:pmem in
          let* obs_addr = check_exp ~mexp:addr ~pexp:paddr in
          let* obs_val = check_exp ~mexp:val_ ~pexp:pval_ in
          Some (obs_mem @ obs_addr @ obs_val @ obs)
        end
      else None
  | _, Store(_,_,_,_,_) -> None
  | Store(_,_,_,_,_), _-> None

  | Load (mem, addr, endian, size), Load (pmem, paddr, pendian, psize) ->
    if (Size.equal size psize && Int.(0 = compare_endian endian pendian)) then
      begin
        let* obs_mem = check_exp ~mexp:mem ~pexp:pmem in
        let* obs_addr = check_exp ~mexp:addr ~pexp:paddr in
        Some (obs_mem @ obs_addr @ obs)
      end
    else None
  | _ , Load (_,_,_,_) -> None
  | Load (_,_,_,_),  _ -> None
  | Let (_, _, _), _ -> failwith "Let should not appear. Not in Flattened Form"
  | _ , Let (_, _ ,_ ) -> failwith "Let should not appear. Not in Flattened Form"

  (* If pattern is Var pv, then pv can match a literal or var*)
  | Int _,   Var pv -> Some (Ob.Var (pv, mrhs) :: obs)
  | Var _,   Var pv -> Some (Ob.Var (pv, mrhs) :: obs)
  | _,   Var _pv -> None
  (* Variables in matchee should only match variables in pattern *)
  | Var _mv, _ -> None
  (* Literal ints only match same literal ints *)
  | _  , Int _p -> if Exp.equal prhs mrhs then Some obs else None
  (* TODO: Other cases
  | Cast of cast * int * exp
  | Let of var * exp * exp
  | Unknown of string * typ
  | Ite of exp * exp * exp
  | Extract of int * int * exp
  | Concat of exp * exp
  *)
  | _, _ -> failwith (Format.sprintf "uncaught case {matchee : %a ; pattern : %a} in Isel.check_def" Exp.pps mrhs Exp.pps prhs)
  in
  obs

(**
  [check_jmp] checks the compatibility of a Jmp match.
*)
let check_jmp (mjmp : Jmp.t) (pjmp : Jmp.t) (env : env) : Ob.t list option =
  let (let*) x f = Option.bind x ~f in
  (* let mdst = Jmp.dst mjmp in
  let pdst = Jmp.dst pjmp in
  let malt = Jmp.alt mjmp in
  let palt = Jmp.alt pjmp in
  let mguard = Jmp.guard mjmp in
  let pguard = Jmp.guard pjmp in
  let* vmap = match mguard, pguard with
  | Some mv, Some pv -> Some {vmap = [mv,pv]}
  | None, None -> Some empty
  | _ ,_ -> None
  in
  let obligations = match mdst, pdst with
  | Some mdst, Some pdst -> {ob with blk_map = [mdst, pdst]}
  | None, None -> obligations
  | _, _ -> None 
  let obligations = match malt, palt with
  | Some mdst, Some pdst -> {ob with blk_map = [mdst, pdst] :: ob.blk_map}
  | None, None -> obligations
  | _, _ -> None *)
  let mkind = Jmp.kind mjmp in
  let pkind = Jmp.kind pjmp in
  let mcond = Jmp.cond mjmp in
  let pcond = Jmp.cond pjmp in 
  (* maybe this is the same as operand above. Should I be concerened that Int don't appear in the match? 
  I could consider them fused in some sense with the insturction maybe
  I should transition to using match_ as obligation
  *)
  let* obs = check_exp ~mexp:mcond ~pexp:pcond in
  match mkind, pkind with
  | Call _mc, Call _pc -> failwith "unsupported call" (* if Int.(compare_call mc pc = 0) then Some empty_obligations else None *)
  | Goto (Direct mtid), Goto (Direct ptid)
  | Ret (Direct mtid), Ret (Direct ptid) ->
      let mblk = Tid.Map.find_exn env.blk_map mtid in
      let pblk = Tid.Map.find_exn env.blk_map ptid in
      Some ((Ob.Blk {mblk; pblk}) :: obs) (* Hmm, it will be hard here to *)
  | Goto (Indirect mexp), Goto (Indirect pexp)
  | Ret (Indirect mexp), Ret (Indirect pexp) ->
      let* obs' = check_exp ~mexp ~pexp in
      Some (obs' @ obs)
  | _, _ -> None

(* Todo? Just for homegeneity *)
let check_var _pv _mexp = Some ()
let check_blk _mblk _pblk = Some ()

(**
  [merge_obligations] performs a loop putting the obligations into the match

  propagate_obligations?
  check_obligations?

Very reptitive and yet I'm not sure how to improve
If you attempt to you get a mess
let add_tid_map
let add_term 

  let check_term ~check ~mterm ~pterm term_match map update = 
  begin
    let ptid = Term.tid pterm in
    match Tid.Map.add map ~key:ptid ~data:term_match with
    | `Ok map -> let* obs' = check mterm pterm in
                 loop (obs' @ obs) (update map)
    | `Duplicate -> let match' = Tid.Map.find_exn jmp_map ptid in
                    let mtid' = Term.tid jmp_match'.mjmp in
                    let mtid = Term.tid mjmp in
                    if Tid.(mtid' = mtid) then loop obs match_ else None
  end
*)
let merge_obligations obs match_ env : match_ option =
  let (let*) x f = Option.bind x ~f in
  let rec loop obs match_ =
    match obs with
    | [] -> Some match_
    | (Ob.Var (pv, mexp)) :: obs ->
      begin
        let vmap = match_.vmap in
        match Var.Map.add vmap ~key:pv ~data:mexp with
        | `Ok vmap -> let* () = check_var pv mexp in
                      loop obs {match_ with vmap}
        | `Duplicate -> let e' = Var.Map.find_exn vmap pv in
                        if Exp.(mexp = e') then loop obs match_ else None
      end
    | (Ob.Blk {mblk; pblk}) :: obs ->
      begin
        let blk_map = match_.blk_map in
        let ptid = Term.tid pblk in
        match Tid.Map.add blk_map ~key:ptid ~data:{mblk; pblk} with
        | `Ok blk_map -> let* () = check_blk mblk pblk in
                        loop obs {match_ with blk_map}
        | `Duplicate -> let blk_match' = Tid.Map.find_exn blk_map ptid in
                        let mtid' = Term.tid blk_match'.mblk in
                        let mtid = Term.tid mblk in
                        if Tid.(mtid' = mtid) then loop obs match_ else None
      end
    | (Ob.Def ({mdef; pdef} as def_match)) :: obs ->
      begin
        let def_map = match_.def_map in
        let ptid = Term.tid pdef in
        match Tid.Map.add def_map ~key:ptid ~data:def_match with
        | `Ok def_map -> let* obs' = check_def mdef pdef in
                         loop (obs' @ obs) {match_ with def_map}
        | `Duplicate -> let def_match' = Tid.Map.find_exn def_map ptid in
                        let mtid' = Term.tid def_match'.mdef in
                        let mtid = Term.tid mdef in
                        if Tid.(mtid' = mtid) then loop obs match_ else None
      end
    | (Ob.Jmp ({pjmp; mjmp} as jmp_match)) :: obs ->
      begin
        let jmp_map = match_.jmp_map in
        let ptid = Term.tid pjmp in
        match Tid.Map.add jmp_map ~key:ptid ~data:jmp_match with
        | `Ok jmp_map -> let* obs' = check_jmp mjmp pjmp env in
                        loop (obs' @ obs) {match_ with jmp_map}
        | `Duplicate -> let jmp_match' = Tid.Map.find_exn jmp_map ptid in
                        let mtid' = Term.tid jmp_match'.mjmp in
                        let mtid = Term.tid mjmp in
                        if Tid.(mtid' = mtid) then loop obs match_ else None
      end
  in
  loop obs match_


(* [match_bir] returns all possible matches of a pattern with a matchee.
   It generates candidates and then filters them. *)
let match_bir ~matchee:(matchee : Blk.t list)
              ~pat:(pat : Blk.t list) : match_ list =
  (* We use List based nondeterminism for search *)
  let (let*) x f = List.bind ~f x in
  let blk_tid_map = List.map matchee ~f:(fun blk -> (Term.tid blk, blk)) in
  let env = { blk_map = Tid.Map.of_alist_exn blk_tid_map  } in
  let matches = List.fold pat ~init:[empty_match_]
    ~f:(fun matches pblk ->
     (* Guess block from matchee *)
     let* mblk = matchee in
     (* Add to each match_ *)
     let matches = List.filter_map matches ~f:(fun match_ ->
              merge_obligations [Ob.Blk {mblk; pblk}] match_ env)
          
     in
     (* Enumerate all possible def correspondences within blk correspondence *)
     let mdefs = Term.enum def_t mblk |> Seq.to_list in
     let pdefs = Term.enum def_t pblk |> Seq.to_list in
     (* Add to each match_ *)
     let matches = List.fold pdefs ~init:matches ~f:(fun matches pdef ->
        let* mdef = mdefs in
        List.filter_map matches ~f:(fun match_ -> 
          merge_obligations [Ob.Def {pdef; mdef}] match_ env)
        )
     in
     (* Enumerate all possible jmp correspondences within blk correspondence *)
    let mjmps = Term.enum jmp_t mblk |> Seq.to_list in
    let pjmps = Term.enum jmp_t pblk |> Seq.to_list in
    let matches =
        List.fold pjmps ~init:matches ~f:(fun matches pjmp ->
          let* mjmp = mjmps in
          List.filter_map matches ~f:(fun match_ ->
            merge_obligations [Ob.Jmp {pjmp; mjmp}] match_ env)
        )
    in
    matches
      (* TODO: Generate phi correspondences *)
      (* Perhaps enumerate over all variables at the end? *)
    )
  in
  matches

let build_matches matchee (pats : Blk.t list String.Map.t) =
  String.Map.map pats ~f:(fun pat ->
    match_bir ~matchee ~pat
    )


module Serial = struct
  open Minizinc_utils
  (* TODO: These should be moved into Minizinc.ml *)
  (* Phantom types make yojson_deriving produce function with unused variables.
   This sets off a warning *)
  let mzn_map_of_yojson = fun _ -> [%of_yojson: 'b list]
  let mzn_map_to_yojson = fun _ -> [%to_yojson: 'b list]
  let mzn_set_of_list l = {set = l}
  let mzn_enum (x : string) : mzn_enum = {e = x}

  type operand = mzn_enum [@@deriving yojson] (* datum? *)
  type operation = mzn_enum [@@deriving yojson]
  type match_id = int [@@deriving yojson]
  type match_serial = {
    numMatches : int;
    allOperationsInFunction : mzn_enum_def;
    allDataInFunction : mzn_enum_def;
    operationsCoveredByMatch : (match_id, operation mzn_set) mzn_map;
    operandsDefinedByMatch : (match_id, operand mzn_set) mzn_map;
  } [@@deriving yojson]
  let empty = {
    numMatches = 0;
    allOperationsInFunction  = mzn_set_of_list [];
    allDataInFunction = mzn_set_of_list [];
    operationsCoveredByMatch = [];
    operandsDefinedByMatch  = [];
  }

  let covered_ops ({def_map; _} : match_) =
    let defs = Tid.Map.data def_map in
    let mtids = List.map ~f:(fun {mdef; _} -> Term.tid mdef) defs in
    Tid.Set.of_list mtids |> Tid.Set.to_list

  let defines_vars ({def_map; _} : match_) : Var.t list =
    (* TODO: Check phi nodes *)
    Tid.Map.fold def_map ~init:Var.Set.empty ~f:(fun ~key:_ ~data:{mdef; _} acc ->
      Var.Set.add acc (Def.lhs mdef)
  ) |> Var.Set.to_list
  let operand_of_var (v : Var.t) : operand = mzn_enum (Var.to_string v)
  let operation_of_tid (t : Tid.t) : operation = mzn_enum (Tid.to_string t)
  let serial_of_matches (matches : match_ list) serial : match_serial =
    (* TODO: replace to_string functions with Sexp functions? *)
    {
     serial with
     numMatches = List.length matches + 1; (* The 1 accounts for the null def patterns *)
     operationsCoveredByMatch = List.map matches ~f:(fun match_ -> {set = List.map ~f:operation_of_tid (covered_ops match_)});
     operandsDefinedByMatch = List.map matches ~f:(fun match_ -> mzn_set_of_list @@ List.map ~f:operand_of_var (defines_vars match_)  )
    }
  let all_vars (blk : Blk.t) : Var.Set.t =
    let vset = Var.Set.empty in
    let fold_cls vset cls f = Seq.fold (Term.enum cls blk) ~init:vset ~f:(fun acc c ->
      Var.Set.union acc (f c)
      ) in
    let vset = fold_cls vset jmp_t Jmp.free_vars in
    let vset = fold_cls vset phi_t
        (fun phi -> Var.Set.add (Phi.free_vars phi) (Phi.lhs phi))  in
    let vset = fold_cls vset def_t
        (fun def -> Var.Set.add (Def.free_vars def) (Def.lhs def))  in
    vset
  
  let all_vars_blks (blks : Blk.t list) : Var.Set.t = 
    List.fold blks ~init:Var.Set.empty ~f:(fun acc blk -> 
      Var.Set.union acc (all_vars blk )
    )
  let serial_of_blks (matchee : Blk.t list) serial : match_serial =
    let vars = Var.Set.to_list @@ all_vars_blks matchee in
    let tids = List.concat_map matchee ~f:(fun blk -> Term.enum def_t blk |> Seq.to_list)  in
    let tids = List.map ~f:Term.tid tids in
    {serial with
      allOperationsInFunction = mzn_set_of_list @@ List.map ~f:operation_of_tid tids;
      allDataInFunction = mzn_set_of_list @@ List.map ~f:operand_of_var vars
    }

  (*
     The null definition pattern is a default pattern that covers externally defined variables.
     This pattern will be selected to cover/define them.
     We will arbitrarily use match number 0 as this pattern.
     We must be careful to remove this when deserializing the solution.
  *)
  let null_def_pattern matchee serial =
    let free_vars = Sub.free_vars (Sub.create ~blks:matchee ()) in
    let free_operands = mzn_set_of_list @@ List.map ~f:operand_of_var @@ Var.Set.to_list free_vars in
    {
      serial with
      operationsCoveredByMatch = mzn_set_of_list [] :: serial.operationsCoveredByMatch;
      operandsDefinedByMatch = free_operands :: serial.operandsDefinedByMatch
    }

  let build_serial matchee matches =
    let serial = empty in
    let serial = serial_of_matches matches serial in
    let serial = null_def_pattern matchee serial in
    let serial = serial_of_blks matchee serial in
    match_serial_to_yojson serial

    type sol_serial =
    {
      sel : (match_id, bool) mzn_map;
      omatch : (operation, match_id) mzn_map;
      dmatch : (operand, match_id) mzn_map;
      _objective : int
    } [@@deriving yojson]
end (* Serial *)
end (* Pattern *)

module Template = struct
  type t = Ir.t
  let instantiate_ir_template ({vmap; blk_map; _} : Pattern.match_) (template : t) : Ir.t =
    (* freshen up unique ids of operands and operations in templates *)
    let template = Ir.freshen_operands template in
    let template = Ir.freshen_operation_ids template in
    (* Give proper block ids to templates *)
    let template = Ir.map_blks template ~f:(fun blk ->
      let {mblk; _} : Pattern.blk_match = Tid.Map.find_exn blk_map blk.id in
      {blk with id = Term.tid mblk}
      ) in
    (* Map variables to proper operands*)
    let template = Ir.map_operands template ~f:(fun operand ->
      match operand with
      | Var opvar -> begin
          let temp = List.hd_exn opvar.temps in
          let mexp : exp = Var.Map.find_exn vmap temp in
          match mexp with
          | Var v -> Var {opvar with temps = [v]} (* Var could be Void? maybe check physicality of variable? *)
          | Int i -> Const i (* failwith "need to deal with int" *)
          | _ -> failwith (Format.sprintf
                 "Isel.instantiate_ir_template: Variable matched to non variable or literal %a"
                 Exp.pps mexp)
          end
      | _ -> failwith "undealt with operand case"
      ) 
    in
    template


end

module Utils = struct
  let memvar (name : string) : Var.t = Var.create name (Mem (Size.addr_of_int_exn 64, Size.r8))
  let var64 (name : string) : Var.t  = Var.create name (Imm 64)
  let def_pat p = [Blk.create ~defs:[p] ()]
  let x = var64 "x"
  let y = var64 "y"
  let z = var64 "z"
  let (:=) lhs rhs = Def.create lhs rhs
  let binop_pat op = def_pat (z := Bil.binop op (Bil.var x) (Bil.var y))

  let binop_template pat opcode : Ir.t =
    let blkid = List.hd_exn pat |> Term.tid in
    let operand v = Ir.Var (Ir.simple_var v) in
    let operation = Ir.simple_op opcode (operand z) [operand x; operand y] in
    { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
     congruent = []
    }

  let store_pat_temp : Pattern.pat * Ir.t = 
      let mem = memvar "mem" in
      let mem' = memvar "mem'" in
      let addr = var64 "addr" in
      let value = var64 "val" in
      let pat = Blk.create ~defs:Bil.(
        let (:=) lhs rhs = Def.create lhs rhs in
      [
            mem' := store ~mem:(var mem) ~addr:(var addr) (var value) LittleEndian Size.r64;
      ]) () 
      in
      let template : Ir.t = 
        let blkid = Term.tid pat in 
        let operand v = Ir.Var (Ir.simple_var v) in
        let operation = Ir.simple_op (Ir.Opcode.create "str") 
              (operand mem') [operand mem; operand addr; operand value] in
        { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
          congruent = []}
      in
      ([pat], template)
    let store_pat : Pattern.t = let (pat, _) = store_pat_temp in pat
    let store_template : Template.t = let (_, template) = store_pat_temp in template

    let load_pat_temp : Pattern.t * Template.t =
      let mem = memvar "mem" in
      let addr = var64 "addr" in
      let value = var64 "val" in
      let pat = Blk.create ~defs:Bil.(
    let (:=) lhs rhs = Def.create lhs rhs in
    [
    value := load ~mem:(var mem) ~addr:(var addr) LittleEndian Size.r64;
    ]) () in
    let template : Ir.t = 
      let blkid = Term.tid pat in 
      let operand v = Ir.Var (Ir.simple_var v) in
      let operation = Ir.simple_op (Ir.Opcode.create "ld") 
            (operand value) [operand mem; operand addr] in
      { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
        congruent = []}
    in
    ([pat], template)

    let load_pat : Pattern.t = let (pat, _) = load_pat_temp in pat
    let load_template : Template.t = let (_, template) = load_pat_temp in template

end

(* TODO: Really, merge_blk should fuse remove anything that is in the ins and outs? *)
let merge_blk (blk1 : Ir.blk) (blk2 : Ir.blk) : Ir.blk = 
  assert (Tid.equal blk1.id blk2.id);
  assert ((List.length blk1.ctrl) + (List.length blk2.ctrl) <= 1);
  {
    id = blk1.id;
    data = List.append blk1.data blk2.data;
    ctrl = List.append blk1.ctrl blk2.ctrl;
    ins = {blk1.ins with lhs = List.append blk1.ins.lhs blk2.ins.lhs};
    outs = {blk1.outs with operands = List.append blk1.outs.operands blk2.outs.operands};
    frequency = blk1.frequency + blk2.frequency
  }
let merge_ir (vir1 : Ir.t) (vir2 : Ir.t) : Ir.t =
  let blkmap1 = Tid.Map.of_alist_exn (List.map vir1.blks ~f:(fun blk -> (blk.id, [blk]))) in
  let blkmap = List.fold vir2.blks ~init:blkmap1 ~f:(fun acc blk2 ->
      Tid.Map.add_multi acc ~key:(blk2.id) ~data:blk2
    ) in
  let blkmap = Tid.Map.map blkmap ~f:(fun blks -> List.reduce blks ~f:merge_blk |> Option.value_exn ) in
  let blks = Tid.Map.data blkmap in
  {
    blks;
    congruent = List.append vir1.congruent vir2.congruent
  }

let run
  ~isel_model_filepath:(isel_model_filepath : string)
  (matchee : Blk.t list)
  (pats : Pattern.t String.Map.t) 
  (templates : Template.t String.Map.t) =
  Events.(send @@ Info "Running Instruction Selector.\n");
  let open Knowledge.Syntax in
  let matches = Pattern.build_matches matchee pats in
  let match_templates = String.Map.mapi matches ~f:(fun ~key ~data ->
    let template = String.Map.find_exn templates key in
    List.map data ~f:(fun match_ -> ( match_ , Template.instantiate_ir_template match_ template ))
  ) in
  let match_templates = String.Map.to_alist match_templates in
  let match_templates = List.concat_map match_templates ~f:(fun (_name, matches) -> matches) in
  let matches = List.map match_templates ~f:(fun (match_, _template) -> match_) in
  let params = Pattern.Serial.build_serial matchee matches in
  let* sol_json = Minizinc_utils.run_minizinc ~model_filepath:isel_model_filepath params in
  let* sol_serial = match Pattern.Serial.sol_serial_of_yojson sol_json with
    | Ok sol_serial -> KB.return sol_serial
    | Error msg -> Kb_error.fail (Kb_error.Minizinc_deserialization msg) in
  let good_templates = List.zip_exn (List.tl_exn sol_serial.sel) match_templates |>
        List.filter_map ~f:(fun (use,(_, template)) -> if use then Some template else None) in
  let vir = List.reduce ~f:merge_ir good_templates in
  let vir = Option.value ~default:Ir.empty vir in
  KB.return vir