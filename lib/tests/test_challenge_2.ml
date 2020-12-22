open !Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_vibes
open Bap_core_theory
open Theory

module Prog (C : Core) = struct

  open C
  open Core_notations.Make(C)

  let mem = Var.define (Mem.define (Bitv.define 32) (Bitv.define 32)) "mem"
  let load loc = load (var mem) loc
  let store loc value = (mem := store (var mem) loc value)

  let int32 w = int (Bitv.define 32) (Bitvec.M32.int w)
  let (!$) s = int (Bitv.define 32) (Bitvec.M32.(!$ s))
  let true_word = !$ "1"
  let false_word = !$ "0"

  let speed_value = Var.define (Bitv.define 32) "speed_value"
  let brake_switch = Var.define (Bitv.define 32) "brake_switch"
  (* This is the address of the record "bumper" *)
  (* The layout is as follows: *)
  (* {
   *   ...
   *   prev_brake_state : Word @ 7
   *   brake_state      : Word @ 8
   *   flash_lock       : Word @ 9
   *   flash_timer      : Word @ 10
   * } *)
  let bumper = Var.define (Bitv.define 32) "brake_switch"
  (* This is the address of the char array "buff" *)
  let buff = Var.define (Bitv.define 32) "buff"

  let prev_brake_state = int32 7
  let brake_state = int32 8
  let flash_lock = int32 9
  let flash_timer = int32 10

  let entry_point = Tid.for_name "entry_point"
  let brake_state_true = Tid.for_name "brake_state_true"
  let speed_value_pos =  Tid.for_name "speed_value_pos"
  let brake_state_false = Tid.for_name "brake_state_false"
  let branch_exit = Tid.for_name "branch_exit"


  (* uint16_t speed_value;  // fixed here
   * uint8_t brake_switch;
   * // Extract Speed and brake switch from frame
   * speed_value  = (buff[3] << 8) + buff[2];  // buf[3] = speed integer, buf[2] = speed decimal
   * brake_switch = (buff[4] & 0b00001100) >> 2;
   * // update related bumper members
   * bumper->brake_state = (brake_switch) ? true : false;
   *
   * // This segment would ideally be moved to bumper method
   * if (bumper->brake_state) { *)
  let blk1 =
    let data =
      data_body
        [
          speed_value := load (var buff + int32 3) << int32 8;
          brake_switch := (load (var buff + int32 4) & !$ "0b00001100") >> int32 2;
          store (var bumper + brake_state)
            (ite (var brake_switch |> non_zero) true_word false_word);
        ]
    in
    let control =
      ctrl_body
        [
          branch (load (var bumper + brake_state) |> non_zero)
            (goto brake_state_true)
            (goto brake_state_false);
        ]
    in
    blk entry_point data control

  (* if ((speed_value > 0) && (bumper->prev_brake_state != bumper->brake_state)){  // speed *) 
  let blk2 =
    let data = data_body [] in
    let control =
      let cond = (ugt (var speed_value) (int32 0)) &&
                   (load (var bumper + prev_brake_state) != load (var bumper + brake_state)) in
      ctrl_body [ branch cond (goto speed_value_pos) (goto branch_exit); ]
    in
    blk brake_state_true data control

  (* bumper->flash_lock = true;
   * bumper->flash_timer = 0; *)
  let blk3 =
    let data =
      data_body
        [
          store (var bumper + flash_lock) true_word;
          store (var bumper + flash_timer) (int32 0);
        ]
    in
    let control = ctrl_body [ goto branch_exit ] in
    blk speed_value_pos data control


  (* bumper->flash_lock = false; *)
  let blk4 =
    let data = data_body [ store (var bumper + flash_lock) false_word ] in
    let control = ctrl_body [ goto branch_exit ] in
    blk brake_state_false data control

  (* bumper->prev_brake_state = bumper->brake_state; *)
  let blk5 =
    let data = data_body
                 [ store (var bumper + prev_brake_state) (load (var bumper + brake_state)) ] in
    let control = ctrl_body [] in
    blk branch_exit data control


  let prog =
    (* FIXME: is [top] correct here? *)
    let empty = perform Effect.Sort.top in
    List.fold ~init:empty ~f:seq [blk1; blk2; blk3; blk4; blk5]

end
