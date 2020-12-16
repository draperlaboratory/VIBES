open !Core_kernel
open Bap.Std

type op_var = {
  id : Var.t;
  temps : Var.t list;
  pre_assign : Var.t option
} [@@deriving compare, equal]

let simple_var v = {id = v; temps = []; pre_assign = None}

type operand = Var of op_var | Const of Word.t [@@deriving compare, equal]

type operation = {
  id : Tid.t;
  lhs : operand;
  insns : Arm_insn.t list;
  optional : bool;
  operands :  operand list;
} [@@deriving compare, equal]

let simple_op opcode arg args =
  let tid = Tid.create () in
  { id = tid;
    lhs = arg;
    insns = [opcode];
    optional = false;
    operands = args;
  }

type blk = {
  id : Tid.t;
  operations : operation list;
  ins : Var.Set.t;
  outs : Var.Set.t;
  frequency : int
} [@@deriving compare, equal]


let simple_blk tid ops =
  {
    id = tid;
    operations = ops;
    (* Probably we should just add every variable in ops here *)
    ins = Var.Set.empty;
    outs = Var.Set.empty;
    frequency = 1
  }


type t = {
  blks : blk list;
  congruent : (operand * operand) list
} [@@deriving compare, equal]

let empty = {blks = []; congruent = []}

let union t1 t2 =
  let comp_pair = Tuple.T2.compare ~cmp1:compare_operand ~cmp2:compare_operand in
  {
    blks =
      List.dedup_and_sort ~compare:compare_blk (t1.blks @ t2.blks);
    congruent =
      List.dedup_and_sort ~compare:comp_pair (t1.congruent @ t2.congruent)
  }

let add blk t =
  {t with blks = blk::t.blks}
