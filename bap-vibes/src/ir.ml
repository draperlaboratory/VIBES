open !Core_kernel
open Bap.Std


module type ARCH = sig

  type reg [@@deriving sexp, equal, compare]

  type cond [@@deriving sexp, equal, compare]

  type insn [@@deriving sexp, equal, compare]

  val fp : reg

  val pc : reg

  val dummy : reg

  val cond_to_string : cond -> string

end


module Make (M : ARCH) = struct

  type reg = M.reg [@@deriving compare, equal, sexp]

  let equal_reg a b = compare_reg a b = 0

  type op_var = {
    id : Var.t;
    temps : Var.t list;
    pre_assign : reg option
  } [@@deriving compare, sexp]

  let equal_op_var x y = [%equal: Var.t] x.id y.id

  let simple_var v =
    let pre_assign =
      if String.(Var.name v = "FP") then
        Some M.fp
      else if String.(Var.name v = "PC") then
        Some M.pc
      else
        None
    in
    {
      id = Var.create ~fresh:true "operand" (Var.typ v);
      temps = [v];
      pre_assign = pre_assign
    }

  let given_var v reg =
    {
      id = Var.create ~fresh:true "operand" (Var.typ v);
      temps = [v];
      pre_assign = Some reg
    }

  type cond = M.cond [@@deriving compare, sexp]

  let equal_cond a b = compare_cond a b = 0

  type operand =
      Var of op_var
    | Const of Word.t
    | Label of Tid.t
    | Cond of cond
    | Void
    | Offset of Word.t [@@deriving compare, equal, sexp]

  type shift = [
    | `ASR
    | `LSL
    | `LSR
    | `ROR
    | `RRX
  ] [@@deriving sexp, equal, compare]


  let freshen_operand o =
    match o with
    | Var v ->
      let fresh_v =
        { v with
          id =
            Var.create
              ~is_virtual:true
              ~fresh:true
              (Var.name v.id)
              (Var.typ v.id)
        } in
      Var fresh_v
    | _ -> o


  let op_var_exn (x : operand) : op_var =
    match x with
    | Var o -> o
    | _ -> failwith "Expected op_var"

  type insn = M.insn [@@deriving compare, sexp, equal]

  type operation = {
    id : Tid.t;
    lhs : operand list;
    insns : insn list;
    optional : bool;
    operands :  operand list;
  } [@@deriving compare, equal, sexp]

  let simple_op opcode arg args =
    let tid = Tid.create () in
    { id = tid;
      lhs = [arg];
      insns = [opcode];
      optional = false;
      (* Operands need to have unique ids *)
      operands = List.map ~f:freshen_operand args;
    }

  let mk_empty_operation () =
    let tid = Tid.create () in
    { id = tid;
      lhs = [];
      insns = [];
      optional = false;
      operands = [];
    }

  type blk = {
    id : Tid.t;
    data : operation list;
    ctrl : operation list;
    ins : operation;
    outs : operation;
    frequency : int
  } [@@deriving compare, equal, sexp]


  let simple_blk tid ~data ~ctrl =
    {
      id = tid;
      data= data;
      ctrl = ctrl;
      (* Probably we should just add every variable in ops here *)
      ins = mk_empty_operation ();
      outs = mk_empty_operation ();
      frequency = 1
    }


  type t = {
    blks : blk list;
    congruent : (op_var * op_var) list
  } [@@deriving compare, equal, sexp]

  let empty = {blks = []; congruent = []}

  let union t1 t2 =
    let comp_pair = Tuple.T2.compare ~cmp1:compare_op_var ~cmp2:compare_op_var in
    {
      blks =
        List.dedup_and_sort ~compare:compare_blk (t1.blks @ t2.blks);
      congruent =
        List.dedup_and_sort ~compare:comp_pair (t1.congruent @ t2.congruent)
    }

  let add blk t =
    {t with blks = blk::t.blks}


  let operation_to_string (o : operation) = Tid.to_string o.id
  let op_var_to_string (o : op_var) = Var.to_string o.id

  let var_operands (ops : operand list) : op_var list =
    List.fold ~f:(fun acc o ->
        match o with
        | Var v -> v :: acc
        | Const _ | Label _ | Cond _ | Void | Offset _ -> acc
      ) ~init:[] ops

  module Blk = struct

    let all_operands (blk : blk) : operand list =
      let operation_operands op_list =
        List.concat_map op_list
          ~f:(fun operation ->
              operation.lhs @ operation.operands)
      in
      blk.ins.lhs @
      blk.outs.operands @
      (operation_operands blk.data) @
      (operation_operands blk.ctrl)

    let all_rhs_operands (blk : blk) : operand list =
      let operation_operands op_list =
        List.concat_map op_list
          ~f:(fun operation ->
              operation.operands)
      in
      blk.ins.operands @
      blk.outs.operands @
      (operation_operands blk.data) @
      (operation_operands blk.ctrl)

    let all_lhs_operands (blk : blk) : operand list =
      let operation_operands op_list =
        List.concat_map op_list
          ~f:(fun operation ->
              operation.lhs)
      in
      blk.ins.lhs @ blk.outs.lhs @ (operation_operands blk.data) @ (operation_operands blk.ctrl)

    let all_temps (blk : blk) : Var.Set.t =
      List.concat_map (all_operands blk)
        ~f:(fun op ->
            match op with
            | Const _ | Label _ | Cond _ | Void | Offset _ -> []
            | Var op -> op.temps) |>
      Var.Set.of_list

    let definer_map (blk : blk) : op_var Var.Map.t =
      List.fold
        (all_lhs_operands blk |> var_operands)
        ~init:Var.Map.empty
        ~f:(fun acc operand ->
            List.fold operand.temps
              ~init:acc
              ~f:(fun acc tmp ->
                  Var.Map.add_exn acc ~key:tmp ~data:operand))

    let users_map (blk : blk) : (op_var list) Var.Map.t =
      List.fold (all_rhs_operands blk |> var_operands)
        ~init:Var.Map.empty
        ~f:(fun acc operand ->
            List.fold operand.temps ~init:acc ~f:(fun acc temp ->
                Var.Map.update acc temp
                  ~f:(fun mrandlist ->
                      match mrandlist with
                      | Some operandlist -> operand :: operandlist
                      | None -> [operand])
              )
          )

    let all_operations (blk : blk) : operation list =
      blk.ins :: ( blk.outs :: (blk.data @ blk.ctrl))

    let operation_insn (blk : blk) : (insn list) Tid.Map.t =
      List.fold ~init:Tid.Map.empty ~f:(fun acc o ->
          Tid.Map.add_exn acc ~key:o.id ~data:o.insns
        ) (all_operations blk)

    let operand_operation (blk : blk) : operation Var.Map.t =
      let alist =
        List.concat_map (all_operations blk)
          ~f:(fun operation ->
              let rhs_operands =
                List.map (var_operands operation.operands)
                  ~f:(fun op -> (op.id, operation))
              in
              let lhs_operands =
                List.map (var_operands operation.lhs)
                  ~f:(fun op -> (op.id, operation)) in
              lhs_operands @ rhs_operands)
      in
      Var.Map.of_alist_exn alist

  end


  let var_map_union_exn m1 m2 =
    Var.Map.merge m1 m2
      ~f:(fun ~key:_ ab ->
          match ab with
          | `Left a -> Some a
          | `Right b -> Some b
          | `Both(_ , _) -> failwith "Map has both keys")

  let tid_map_union_exn m1 m2 =
    Tid.Map.merge m1 m2
      ~f:(fun ~key:_ ab ->
          match ab with
          | `Left a -> Some a
          | `Right b -> Some b
          | `Both(_ , _) -> failwith "Map has both keys")

  let map_blks ~f (sub : t) : t =
    {blks = List.map ~f sub.blks; congruent = sub.congruent}

  let map_operations ~f (vir : t) : t =
    map_blks vir
      ~f:(fun b ->
          { id = b.id;
            data = List.map ~f b.data;
            ctrl = List.map ~f b.ctrl;
            ins = f b.ins;
            outs = f b.outs;
            frequency = b.frequency })

  let map_op_vars ~f (vir : t) : t =
    let f2 o = match o with
      | Var o -> Var (f o)
      | o -> o
    in
    let apply_to_op (o : operation) : operation =
      {
        o with
        lhs = List.map ~f:f2 o.lhs;
        operands = List.map ~f:f2 o.operands;
      }
    in
    {
      blks =
        List.map vir.blks
          ~f:(fun b ->
              {
                b with
                data = List.map ~f:apply_to_op b.data;
                ctrl = List.map ~f:apply_to_op b.ctrl;
                ins = apply_to_op b.ins;
                outs = apply_to_op b.outs;
              }
            );
      congruent = List.map ~f:(Tuple2.map ~f:f) vir.congruent;
    }

  let all_temps (sub : t) : Var.Set.t =
    List.fold sub.blks
      ~init:Var.Set.empty
      ~f:(fun acc blk ->
          Var.Set.union acc (Blk.all_temps blk))

  let all_operands (sub : t) : Var.Set.t =
    List.concat_map sub.blks ~f:Blk.all_operands |>
    var_operands |>
    List.map ~f:(fun (o : op_var) -> o.id) |>
    Var.Set.of_list

  let preassign_map (sub : t) : (reg option) Var.Map.t =
    List.concat_map sub.blks ~f:Blk.all_operands |>
    var_operands |>
    List.map ~f:(fun op -> (op.id, op.pre_assign))
    |> Var.Map.of_alist_exn


  let definer_map (sub : t) : op_var Var.Map.t =
    List.fold sub.blks
      ~init:Var.Map.empty
      ~f:(fun acc blk ->
          var_map_union_exn acc (Blk.definer_map blk))


  let users_map (sub : t) : (op_var list) Var.Map.t =
    List.fold sub.blks
      ~init:Var.Map.empty
      ~f:(fun acc blk ->
          let m = Blk.users_map blk in
          Var.Map.merge acc m
            ~f:(fun ~key:_ ab ->
                match ab with
                | `Left a -> Some a
                | `Right b -> Some b
                | `Both(a,b) -> Some (a @ b)))

  let temp_blk (sub : t) : Tid.t Var.Map.t =
    List.concat_map sub.blks
      ~f:(fun blk ->
          List.map (Blk.all_temps blk |> Var.Set.to_list)
            ~f:(fun t -> (t, blk.id))) |>
    Var.Map.of_alist_exn

  let operation_insns (sub : t) : (insn list) Tid.Map.t =
    List.fold sub.blks
      ~init:Tid.Map.empty
      ~f:(fun acc blk ->
          tid_map_union_exn acc (Blk.operation_insn blk))

  let operand_operation (sub : t) : operation Var.Map.t =
    List.fold sub.blks
      ~init:Var.Map.empty
      ~f:(fun acc blk ->
          var_map_union_exn acc (Blk.operand_operation blk))


  let pretty_operand o =
    match o with
    | Var o ->
      sprintf "%s : %s < %s"
        (Var.to_string o.id)
        (List.map ~f:Var.to_string o.temps |> String.concat ~sep:"::")
        ((Option.map
            ~f:(fun r ->
                M.sexp_of_reg r |>
                Ppx_sexp_conv_lib.Sexp.to_string) o.pre_assign) |>
         Option.value ~default:"N/A")
    | Const c -> Word.to_string c
    | Label l -> Tid.to_string l
    | Cond c -> M.cond_to_string c
    | Void -> ""
    | Offset c -> Format.asprintf "Offset(%d)" (Word.to_int_exn c)

  let pretty_operand_list l =
    List.map ~f:pretty_operand l |> String.concat ~sep:","

  let pretty_operation o =
    sprintf "\t\t[%s]  <- %s [%s]" (pretty_operand_list o.lhs)
      (String.concat
         (List.map o.insns
            ~f:(fun i -> sexp_of_insn i |> Ppx_sexp_conv_lib.Sexp.to_string)))
      (pretty_operand_list o.operands)

  let pretty_blk b = sprintf "blk : %s \n\tins : %s \n\touts: %s\n\tdata: \n%s\n\tctrl: \n%s"
      (Tid.to_string b.id)
      (pretty_operation b.ins)
      (pretty_operation b.outs)
      (List.fold b.data ~init:""
         ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))
      (List.fold b.ctrl ~init:""
         ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))

  let pretty_ir (vir : t) : string =
    List.fold vir.blks ~init:"" ~f:(fun acc b -> acc ^ (pretty_blk b) ^ "\n\n")

  let to_string t = pretty_ir t

  let dummy_reg_alloc t =
    map_op_vars t
      ~f:(fun v ->
          match v.pre_assign with
          | Some _ -> v
          | None -> {v with pre_assign = Some M.dummy (* `R0 *)})

end

module type S = sig

  type reg [@@deriving sexp, equal, compare]

  type cond [@@deriving sexp, equal, compare]

  type insn [@@deriving sexp, equal, compare]


  type op_var = {
    id : var;
    temps : var list;
    pre_assign : reg option
  } [@@deriving compare, equal, sexp]

  val simple_var : var -> op_var

  val given_var : var -> reg -> op_var

  type operand = Var of op_var
               | Const of word
               | Label of tid
               | Cond of cond
               | Void
               | Offset of word [@@deriving compare, equal, sexp]

  type operation = {
    id : tid;
    lhs : operand list;
    insns : insn list;
    optional : bool;
    operands : operand list;
  } [@@deriving compare, equal, sexp]

  val simple_op : insn -> operand -> operand list -> operation

  type blk = {
    id : tid;
    data : operation list;
    ctrl : operation list;
    ins : operation;
    outs : operation;
    frequency : int
  } [@@deriving compare, equal, sexp]

  val simple_blk : tid -> data:(operation list) -> ctrl:(operation list) -> blk

  type t = {
    blks : blk list;
    congruent : (op_var * op_var) list
  } [@@deriving compare, equal, sexp]

  val empty : t

  val union : t -> t -> t

  val add : blk -> t -> t


  val map_blks : f:(blk -> blk) -> t -> t
  val map_op_vars : f:(op_var -> op_var) -> t -> t
  val map_operations : f:(operation -> operation) -> t -> t

  val operation_to_string : operation -> string
  val op_var_to_string : op_var -> string

  val all_temps : t -> Var.Set.t
  val all_operands : t -> Var.Set.t

  val preassign_map : t -> (reg option) Var.Map.t

  val definer_map : t -> op_var Var.Map.t

  val users_map : t -> (op_var list) Var.Map.t

  val temp_blk : t -> Tid.t Var.Map.t

  val operation_insns : t -> insn list Tid.Map.t
  val operand_operation : t -> operation Var.Map.t
  val pretty_ir : t -> string

  val to_string : t -> string

  val op_var_exn : operand -> op_var

  val dummy_reg_alloc : t -> t

end
