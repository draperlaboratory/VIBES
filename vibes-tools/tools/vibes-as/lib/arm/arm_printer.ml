open Core
open Bap.Std
open Bap_core_theory

module Constants = Vibes_constants.Asm
module Ir = Vibes_ir.Types
module Ops = Vibes_select.Arm_ops
module Asm = Types.Assembly
module Patch_info = Vibes_patch_info.Types

type opc = (string, string * string) Either.t

let (let*) x f = Result.bind x ~f
let (let+) x f = Result.map x ~f

let fail msg = Result.fail @@ Errors.Printer_error msg

let opcode
    (name : Ir.opcode)
    ~(is_thumb : bool) : (opc, KB.conflict) result =
  let it n =
    if is_thumb then
      match Ops.Cond.of_string @@ String.drop_prefix name n with
      | None -> Result.return @@ First name
      | Some cc ->
        Result.return @@
        Second ("it " ^ Ops.Cond.to_string cc, name)
    else Result.return @@ First name in
  if String.is_prefix name ~prefix:"mov" then it 3
  else Result.return @@ First name

let tid_to_asm_label (t : tid) : string =
  let drop c = Char.(c = '%' || c = '@') in
  let name = Tid.name t |> String.strip ~drop in
  if Char.is_digit name.[0] then "blk" ^ name else name

type bracket = Open | Close | Neither | Both

let operand
    (op : string)
    (o : Ir.Operand.t)
    (i : int)
    ~(is_loc : bracket) : (string, KB.Conflict.t) result =
  let+ p =
    match o with
    | Var v -> begin
        match v.preassign with
        | None -> fail "operand: operand.pre_assign field is empty"
        | Some v -> Ok (Var.to_string v)
      end
    | Const w ->
      let prefix = match op with
        | "ldr" -> begin
            match is_loc with
            | Neither -> "="
            | Close when i = 3 -> "lsl #"
            | _ -> "#"
          end
        | _ -> "#" in
      Ok (Format.asprintf "%s%a" prefix Word.pp_dec w)
    | Label l -> Ok (tid_to_asm_label l)
    | Void _ -> fail "operand: tried printing a Void operand!"
    | Offset c ->
      Ok (Format.asprintf "(%s + %a - %s)"
            Constants.patch_start_label
            Word.pp_dec c
            Constants.patch_location) in
  match is_loc with
  | Open -> Format.asprintf "[%s" p
  | Close -> Format.asprintf "%s]" p
  | Neither -> p
  | Both -> Format.asprintf "[%s]" p

let rm_void_args : Ir.Operand.t list -> Ir.Operand.t list =
  List.filter ~f:(function
      | Ir.Operand.Void _ -> false
      | _ -> true)

let is_const : Ir.Operand.t -> bool = function
  | Const _ -> true
  | _ -> false

let mk_loc_list
    (op : string)
    (args : Ir.Operand.t list) : (bracket list, KB.conflict) result =
  let len = List.length args in
  let init_neither len = List.init len ~f:(fun _ -> Neither) in
  match op with
  | "ldr" when len = 2 && is_const (List.nth_exn args 1) ->
    Ok [Neither; Neither]
  | "ldr" | "ldrh" | "ldrb" | "str" | "strh" | "strb" ->
    if      len = 2 then Ok [Neither; Both]
    else if len = 3 then Ok [Neither; Open; Close]
    else if len = 4 then Ok [Neither; Open; Neither; Close]
    else fail @@ sprintf
        "mk_loc_list: expected to receive 2 or 3 arguments, \
         got %d (op = %s)" len op
  | _ -> Ok (init_neither len)

let operands
    (op : string)
    (lhs : Ir.Operand.t list)
    (rhs : Ir.Operand.t list) : (string, KB.conflict) result =
  (* `bl` may have pseudo-arguments. *)
  let* rhs =
    if String.(op = "bl") then match rhs with
      | x :: _ -> Ok [x]
      | _ ->
        fail "operands: expected at least 1 argument \
              for the `bl` opcode."
    else Ok rhs in
  (* `cmp` may have pseudo-arguments. *)
  let* rhs =
    if String.(op = "cmp") then match rhs with
      | x :: y :: _ -> Ok [x; y]
      | _ ->
        fail "operands: expected at least 2 arguments \
              for the `cmp` opcode."
    else Ok rhs in
  (* `movcc` may have pseudo-arguments at the end. *)
  let* rhs =
    if String.is_prefix op ~prefix:"mov" &&
       String.length op = 5 then match rhs with
      | x :: _ -> Ok [x]
      | _ ->
        fail "operands: expected at least 1 argument \
              for the `movcc` opcode."
    else Ok rhs in
  let l = rm_void_args (lhs @ rhs) in
  let* is_loc_list = mk_loc_list op l in
  List.zip_exn is_loc_list l |>
  List.mapi ~f:(fun i (is_loc, o) ->
      operand op o i ~is_loc) |>
  Result.all |> Result.map ~f:(fun s ->
      List.intersperse s ~sep:", " |>
      String.concat)

let operation
    (t : Ir.Operation.t)
    ~(is_thumb : bool) : (string list, KB.conflict) result =
  let* op = opcode (List.hd_exn t.opcodes) ~is_thumb in
  let o = match op with
    | First o -> o
    | Second (_, o) -> o in
  let+ ops = operands o t.lhs t.operands in
  match op with
  | First op -> [Format.asprintf "%s %s" op ops]
  | Second (o1, o2) -> [o1; Format.asprintf "%s %s" o2 ops]

let block
    (t : Ir.Block.t)
    ~(is_thumb : bool) : (Types.Assembly.block, KB.conflict) result =
  let+ insns =
    List.map ~f:(operation ~is_thumb) (t.data @ t.ctrl) |>
    Result.all |> Result.map ~f:List.concat in
  let label = Format.asprintf "%s" @@ tid_to_asm_label t.tid in
  Asm.Fields_of_block.create ~label ~insns

let ir ~(is_thumb : bool) : Asm.printer = fun t patch_info ->
  let+ blocks =
    List.map ~f:(block ~is_thumb) t.blks |>
    Result.all in
  let directives = [".syntax unified"] in
  let Patch_info.{patch_point; patch_size; _} = patch_info in
  let patch_point = Bitvec.to_int64 @@ Word.to_bitvec patch_point in
  Asm.Fields.create ~patch_point ~patch_size ~directives ~blocks
