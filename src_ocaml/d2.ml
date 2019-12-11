open Batteries
open Core
open Printf

let get_op opcode = match opcode with
  | 1 -> fun a b -> a + b
  | _ -> fun a b -> a * b

let rec run_intcode_inner state cur_idx =
  let get = BatVect.get state in
  let opcode = get cur_idx in
  if opcode = 99 then
    state
  else
    let op = get_op opcode in
    let input_1_idx = get (cur_idx + 1) in
    let input_2_idx = get (cur_idx + 2) in
    let output_idx = get (cur_idx + 3) in
    let output = op (get input_1_idx) (get input_2_idx) in

    let next_state = BatVect.set state output_idx output in
    run_intcode_inner next_state (cur_idx + 4)

let run_intcode input = run_intcode_inner input 0

let () =
    let start_state =
      let file_input = In_channel.read_lines "../2.txt" |> List.hd_exn in
      let vec_from_file = List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
      (* as per spec: [1] = 12, [2] = 2 *)
      let vec = BatVect.set vec_from_file 1 12 in
      BatVect.set vec 2 2 in
    let end_state = run_intcode start_state in
    print_endline (string_of_int (BatVect.get end_state 0))
