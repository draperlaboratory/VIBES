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

open Core_kernel
open Bap.Std
open Bap_knowledge
module KB = Knowledge
(*-------------------------------------------------------

  It is assumed that the instruction selector is receiving an SSA and flattened BIR
  data structure.

  Flattening the BIR (making the right hand side of a Def a non recursive expression)
  simplifies significantly the problem and brings the data structure in correspondence
  with the nodes of the Blindell universal instruction selection paper.
  [Def.t] become identified with Blindell's computation nodes.

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
        (* We should also check that the types make sense probably *)
        | Var _ | Int _ -> Some ([Var (pv, mexp)])
        | _ -> failwith (sprintf "Isel.check_exp: Matchee not in Flattened Form %a" Exp.pps mexp)
      end
    | Int _i -> if (Exp.equal mexp pexp) then (Some []) else None
    | _ -> failwith (sprintf "Isel.check_exp: Pattern not in Flattened Form %a" Exp.pps pexp)


(*
  TODO or at least be careful.
  [check_def] and [check_jmp] should be checking that the block it belongs to is also
  an obligation. This is fine because of how [match_bir] is structured where it
  only attempts def and jump matches if their blocks are already merged. *)

  (**
     [check_def] given a candidate match between a pattern [Def.t] and matchee
     [Def.t], this will produce None if this candidate match is impossible or
     will produce `Some obligations` with remaining obligations / inferred match
     candidates that should also be checked.
  *)
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
    let mkind = Jmp.kind mjmp in
    let pkind = Jmp.kind pjmp in
    let mcond = Jmp.cond mjmp in
    let pcond = Jmp.cond pjmp in 
    let* obs = check_exp ~mexp:mcond ~pexp:pcond in
    match mkind, pkind with
    | Call _mc, Call _pc -> failwith "unsupported call" (* if Int.(compare_call mc pc = 0) then Some empty_obligations else None *)
    | Goto (Direct mtid), Goto (Direct ptid)
    | Ret (Direct mtid), Ret (Direct ptid) ->
      let mblk = Tid.Map.find_exn env.blk_map mtid in
      let pblk = Tid.Map.find_exn env.blk_map ptid in
      Some ((Ob.Blk {mblk; pblk}) :: obs)
    | Goto (Indirect mexp), Goto (Indirect pexp)
    | Ret (Indirect mexp), Ret (Indirect pexp) ->
      let* obs' = check_exp ~mexp ~pexp in
      Some (obs' @ obs)
    | _, _ -> None

  (* Todo? Just for homegeneous use of [check_*] in [match_bir]. *)
  let check_var _pv _mexp = Some ()
  let check_blk _mblk _pblk = Some ()

  (**
     [merge_obligations] performs a loop putting obligations into a partial match_
     Very repetitive and yet I'm not sure how to improve.
     If you attempt to abstract over the pieces that are in common you get a mess in my opinion.
  *)
  let merge_obligations
      (obs : Ob.t list)
      (match_ : match_)
      (env : env) : match_ option =
    let (let*) x f = Option.bind x ~f in
    (*  [obs] is a worklist of obligations to be checked.
        It is vitally important that one checks in the match_ to see whether this obligation
        has already been discharged before calling a [check_*] function. Otherwise you will
        possibly end up in an infinite loop *)
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
          | `Duplicate  -> let jmp_match' = Tid.Map.find_exn jmp_map ptid in
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
    let env =
      let blk_tid_map blks = List.map blks ~f:(fun blk -> (Term.tid blk, blk)) in
      let pat_blk_map = blk_tid_map pat in
      let matchee_blk_map = blk_tid_map matchee in
      { blk_map = Tid.Map.of_alist_exn (pat_blk_map @ matchee_blk_map) }
    in
    let matches = List.fold pat ~init:[empty_match_]
        ~f:(fun matches pblk ->
            (* Guess block from matchee *)
            let* mblk = matchee in
            (* Add to each match_ *)
            let matches = List.filter_map matches ~f:(fun match_ ->
                merge_obligations [Ob.Blk {mblk; pblk}] match_ env)
            in
            (* The ordering of jumps _does_ matter, so we only allow complete matching on the
               entire ctrl section of the block. This can be considered a more typical match,
               whereas Def.t matching is taking into account a kind of commutativity.
               The ctrl section is a kind of monolithic all or nothing matching.
               No. I should only allow contiguous matches?
               *)
            let pjmps = Term.enum jmp_t pblk |> Seq.to_list in
            let npjmps = List.length pjmps in
            let matches = if Int.(npjmps = 0) then matches
              else begin
                let mjmps = Term.enum jmp_t mblk |> Seq.to_list in
                let nmjmps = List.length mjmps in
                 (* if the size of the pattern is too big to fit in matchee then fail*)
                if Int.(nmjmps < npjmps) then []
                else
                (* generate all contiguous windows of mjmps that are of length npjmp *)
                let rec windows nxs xs acc = if nxs >= npjmps then
                      windows (nxs - 1) (List.tl_exn xs) ((List.take xs npjmps) :: acc)
                    else acc
                in
                let mjmp_windows : Jmp.t list list = windows nmjmps mjmps [] in
                (* let* mjmp_window = mjmp_windows in*)
                let* mjmp_window = mjmp_windows in
                (* fold2_exn should be ok because windows are of length npjmp *)
                let matches = List.fold2_exn pjmps mjmp_window
                    ~init:matches ~f:(fun matches pjmp mjmp ->
                        List.filter_map matches ~f:(fun match_ ->
                            merge_obligations [Ob.Jmp {pjmp; mjmp}] match_ env)
                      )
                in
                matches
              end
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
            matches
            (* TODO: Generate phi correspondences *)
            (* Perhaps enumerate over all variables at the end? Variables should only appear as
               subterms of other pieces, so they shouldbe all covered. *)
          )
    in
    matches

  module Serial = struct
    open Minizinc_utils
    type operand = mzn_enum [@@deriving yojson] (* datum? *)
    type operation = mzn_enum [@@deriving yojson]
    (* Individual matches are identified by their index in the matches list *)
    type match_id = int [@@deriving yojson]
    (* [match_serial] is all the data that the minizinc model needs to run.
       The names are chosen to be in correspondence with the Blindell model
       https://github.com/unison-code/uni-instr-sel/blob/master/solvers/minizinc/base-model.mzn
       - [numMatches] is the total number of matches
       - [allOperationsInFunction]
    *)
    type match_serial = {
      numMatches : int;
      allOperationsInFunction : mzn_enum_def;
      allDataInFunction : mzn_enum_def;
      operationsCoveredByMatch : (match_id, operation mzn_set) mzn_map;
      operandsDefinedByMatch : (match_id, operand mzn_set) mzn_map;
    } [@@deriving yojson]
    let empty : match_serial = {
      numMatches = 0;
      allOperationsInFunction  = mzn_set_of_list [];
      allDataInFunction = mzn_set_of_list [];
      operationsCoveredByMatch = [];
      operandsDefinedByMatch  = [];
    }

    (* [covered_ops] takes a [match_] and returns all the Def.t covered by that
       match_
    *)
    let covered_ops ({def_map; jmp_map; _} : match_) : Tid.t list =
      let defs = Tid.Map.data def_map in
      let jmps = Tid.Map.data jmp_map in
      let mdef_tids = List.map ~f:(fun {mdef; _} -> Term.tid mdef) defs in
      let mjmp_tids = List.map ~f:(fun {mjmp; _} -> Term.tid mjmp) jmps in
      let tids = mdef_tids @ mjmp_tids in
      (* No duplicates. If so, something has gone awry *)
      if Int.(Tid.Set.length (Tid.Set.of_list tids) <> List.length tids) then
        failwith "Isel.covered_ops: Duplicates in tids covered by pattern"
      else ();
      tids
    (* [defines_vars] returns a list of all the variables defined by the match *)
    let defines_vars ({def_map; _} as match_ : match_) : Var.t list =
      (* TODO: Check phi nodes *)
      let defs = Tid.Map.data def_map in
      let def_vars = List.map defs ~f:(fun {mdef;_} -> Def.lhs mdef) in
      (* Not certain this should be allowed. SSA might save us? *)
      if Int.(Var.Set.length (Var.Set.of_list def_vars) <> List.length def_vars) then
        failwith (sprintf "Isel.defines_vars: Duplicates in vars defined by match_: %s"
          (Sexp.to_string_hum @@ sexp_of_match_ match_))
      else ();
      def_vars
    let operand_of_var (v : Var.t) : operand = mzn_enum (Var.to_string v)
    let operation_of_tid (t : Tid.t) : operation = mzn_enum (Tid.to_string t)
    let serial_of_matches matchee (matches : match_ list) serial : match_serial =
      (* TODO: replace to_string functions with Sexp functions? *)
      let free_vars = Sub.free_vars (Sub.create ~blks:matchee ()) in
      let covered_null_def = mzn_set_of_list @@ List.map ~f:operand_of_var @@ Var.Set.to_list free_vars in 
      {
        serial with
        numMatches = 1 + List.length matches;
        operationsCoveredByMatch = mzn_set_of_list [] :: List.map matches ~f:(fun match_ ->
            mzn_set_of_list @@ List.map ~f:operation_of_tid (covered_ops match_));
        operandsDefinedByMatch = covered_null_def :: List.map matches ~f:(fun match_ ->
            mzn_set_of_list @@ List.map ~f:operand_of_var (defines_vars match_))
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

    let all_operations (blk : Blk.t) : Tid.t list =
      let def_tids = Term.enum def_t blk |> Seq.map ~f:Term.tid in
      let jmp_tids = Term.enum jmp_t blk |> Seq.map ~f:Term.tid in
      Seq.append def_tids jmp_tids |> Seq.to_list

    (** [enumerate_nodes] inserts into the serial_match object the identifiers of all
        the operations and operands *)
    let enumerate_nodes (matchee : Blk.t list) (serial : match_serial) : match_serial =
      let vars = Var.Set.to_list @@ all_vars_blks matchee in
      let tids = List.concat_map matchee ~f:all_operations in
      {serial with
       allOperationsInFunction = mzn_set_of_list @@ List.map ~f:operation_of_tid tids;
       allDataInFunction = mzn_set_of_list @@ List.map ~f:operand_of_var vars
      }

    let build_serial (matchee : Blk.t list) (matches : match_ list) : Yojson.Safe.t =
      let serial = empty in
      let serial = serial_of_matches matchee matches serial in
      let serial = enumerate_nodes matchee serial in
      match_serial_to_yojson serial

    type sol_serial =
      {
        sel : (match_id, bool) mzn_map;
        omatch : (operation, match_id) mzn_map;
        dmatch : (operand, match_id) mzn_map;
        _objective : int
      } [@@deriving yojson]

    let filter_templates (sol : sol_serial) (templates : 'a list) =
      (* List.tl_exn to remove null_def match*)
      List.map2_exn (List.tl_exn sol.sel) templates
        ~f:(fun sel temp -> if sel then Some temp else None) |> List.filter_opt

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
    (* TODO: Change tids inside jumps *)
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
        | Label tid ->
            let {mblk; _} : Pattern.blk_match = Tid.Map.find_exn blk_map tid in
            Label (Term.tid mblk)
        | _ -> failwith "[Isel.instantiate_isel_template]: undealt with operand case"
      )
    in
    template

  let _empty : t = Ir.empty

end

type info = (Pattern.t * Template.t) String.Map.t

module Utils = struct
  let memvar (name : string) : Var.t = Var.create name (Mem (Size.addr_of_int_exn 64, Size.r8))
  let var64 (name : string) : Var.t  = Var.create name (Imm 64)
  let def_pat p = [Blk.create ~defs:[p] ()]
  let x = var64 "x"
  let y = var64 "y"
  let z = var64 "z"
  let (:=) lhs rhs = Def.create lhs rhs

  let binop (binop : binop) (opcode : Ir.opcode) : Pattern.t * Template.t =
    let pat = def_pat (z := Bil.binop binop (Bil.var x) (Bil.var y)) in
    let blkid = List.hd_exn pat |> Term.tid in
    let operand v = Ir.Var (Ir.simple_var v) in
    let operation = Ir.simple_op opcode (operand z) [operand x; operand y] in
    pat, { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
           congruent = []
         }

  let mov (opcode : Ir.opcode): Pattern.t * Template.t =
    let pat = def_pat (Def.create z (Bil.var x)) in
    let blkid = Term.tid (List.hd_exn pat) in
    let operand v = Ir.Var (Ir.simple_var v) in
    let operation = Ir.simple_op opcode (operand z) [operand x] in
    pat,
    { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
      congruent = []}

  let store : Pattern.pat * Template.t =
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
    let template : Template.t = 
      let blkid = Term.tid pat in 
      let operand v = Ir.Var (Ir.simple_var v) in
      let operation = Ir.simple_op (Ir.Opcode.create "str") 
          (operand mem') [operand mem; operand addr; operand value] in
      { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
        congruent = []}
    in
    ([pat], template)

  let load : Pattern.t * Template.t =
    let mem = memvar "mem" in
    let addr = var64 "addr" in
    let value = var64 "val" in
    let pat = Blk.create ~defs:Bil.(
        let (:=) lhs rhs = Def.create lhs rhs in
        [
          value := load ~mem:(var mem) ~addr:(var addr) LittleEndian Size.r64;
        ]) () in
    let template : Template.t =
      let blkid = Term.tid pat in
      let operand v = Ir.Var (Ir.simple_var v) in
      let operation = Ir.simple_op (Ir.Opcode.create "ld") 
          (operand value) [operand mem; operand addr] in
      { blks = [Ir.simple_blk blkid ~data:[operation] ~ctrl:[]];
        congruent = []}
    in
    ([pat], template)

    (* Does the pattern require the second block or not? Yes it is required.
       These blocks need to be looked up in env
    *)
    let goto : Pattern.pat * Template.t =
      let jmp_target = Blk.create () in
      let jmp_target_tid = Term.tid jmp_target in
      let jmp_origin = Blk.create ~jmps:[Jmp.create_goto (Direct jmp_target_tid)] () in
      let jmp_origin_tid = Term.tid jmp_origin in
      let pat = [jmp_origin; jmp_target] in
      let template : Template.t =
        let operation = Ir.empty_op () in
        let operation = { operation with
            opcodes = [Ir.Opcode.create "b"];
            operands = [Ir.Label jmp_target_tid]
          }
        in
        { blks = [Ir.simple_blk jmp_origin_tid ~data:[] ~ctrl:[operation]];
          congruent = []}
      in
      (pat, template)
    
    (* A not taken jump *)
    let null_jump : Pattern.pat * Template.t =
      let jmp_target = Blk.create () in
      let jmp_target_tid = Term.tid jmp_target in
      (* let cond = var64 "cond" in *)
      let jmp_origin = Blk.create ~jmps:[Jmp.create_goto ~cond:(Bil.int (Word.zero 1)) (Direct jmp_target_tid)] () in
      let pat = [jmp_origin; jmp_target] in
      let template : Template.t = Ir.empty in
      (pat, template)


end

(** [null_def_match] produces a null definition match and empty no-op instantiated template
    The null definition pattern is a default pattern that covers externally defined variables
    in the matchee. This pattern will be selected to cover/define them, but then the no-op
    Ir chunk is used, which does nothing to the resulting Ir.t when merged
*)
let null_def_match (matchee : Blk.t list) : Pattern.match_ * Ir.t =
  let free_vars = Sub.free_vars (Sub.create ~blks:matchee ()) in
  Format.printf "Free vars %a" Sexp.pp_hum (Var.Set.sexp_of_t free_vars);
  let def_map : Pattern.def_match Tid.Map.t = List.map (Var.Set.to_list free_vars) ~f:(fun x ->
    let def = Def.create x (Unknown ("external", (Var.typ x))) in
    let def_match : Pattern.def_match = {pdef=def; mdef=def} in
    (Term.tid def,def_match))
    |> Tid.Map.of_alist_exn in
  {Pattern.empty_match_ with
   vmap = Var.Set.to_map ~f:(fun x -> Bil.Var x) free_vars;
   (* A dummy def to show that this variable is defined *)
   def_map
  },
  Ir.empty

(* TODO: Really, merge_blk should fuse remove anything that is in the ins and outs? *)
let merge_blk (blk1 : Ir.blk) (blk2 : Ir.blk) : Ir.blk =
  assert (Tid.equal blk1.id blk2.id);
  assert ((List.length blk1.ctrl) + (List.length blk2.ctrl) <= 1);
  {
    id = blk1.id;
    data = List.append blk1.data blk2.data;
    (* TODO: This may not be right. It is unclear how to maintain ctrl block ordering *)
    ctrl = List.append blk1.ctrl blk2.ctrl;
    ins = Ir.empty_op ();
    outs = Ir.empty_op ();
    frequency = 0
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
    (pats : (Pattern.t * Template.t) String.Map.t) : Ir.t Knowledge.t =
  Events.(send @@ Info "Running Instruction Selector.\n");
  let open Knowledge.Syntax in
  let matches = String.Map.map pats ~f:(fun (pat,template) ->
      let matches = Pattern.match_bir ~matchee ~pat in
      List.map matches ~f:(fun match_ ->
          match_, Template.instantiate_ir_template match_ template)
    )
  in
  Events.(send @@ Info "Discovered matches:\n");
  String.Map.iteri matches ~f:(fun ~key ~data ->
    if not (List.is_empty data) then begin
      Events.(send @@ Info (sprintf "Pattern %s:\n" key));
      List.iter data ~f:(fun (_, template) ->
        Events.(send @@ Info (sprintf "%s\n" (Ir.pretty_ir template)))
        )
    end
    else ());
  let match_templates = List.concat_map (String.Map.to_alist matches) ~f:snd in
  (* let match_templates = (null_def_match matchee) :: match_templates in *)
  let matches, templates = List.unzip match_templates in
  let params = Pattern.Serial.build_serial matchee matches in
  let* sol_json = Minizinc_utils.run_minizinc ~model_filepath:isel_model_filepath params in
  let* sol_serial = match Pattern.Serial.sol_serial_of_yojson sol_json with
    | Ok sol_serial -> KB.return sol_serial
    | Error msg -> Kb_error.fail (Kb_error.Minizinc_deserialization msg) in
  let good_templates = Pattern.Serial.filter_templates sol_serial templates in
  let vir = List.reduce ~f:merge_ir good_templates in
  let vir = Option.value ~default:Ir.empty vir in
  KB.return vir