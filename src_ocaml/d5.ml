open Batteries
open Core
open Printf

(* counts the opcode as a parameter *)
let parameter_count opcode = match opcode with
  | 1 -> 4
  | 2 -> 4
  | 3 -> 2
  | 4 -> 2
  | _ -> raise (Failure "Invalid opcode")

let apply_op opcode program_input state cur_idx =
  let get = BatVect.get state in
  (* TODO: get list of inputs here (using parameter modes), then use it for 1 and 2 *)
  match opcode with
  | 1 ->
    let input_1_idx = get (cur_idx + 1) in
    let input_2_idx = get (cur_idx + 2) in
    let output_idx = get (cur_idx + 3) in
    let output = (get input_1_idx) + (get input_2_idx) in
    (BatVect.set state output_idx output)
  | 2 ->
    let input_1_idx = get (cur_idx + 1) in
    let input_2_idx = get (cur_idx + 2) in
    let output_idx = get (cur_idx + 3) in
    let output = (get input_1_idx) * (get input_2_idx) in
    (BatVect.set state output_idx output)
  | 3 ->
    let output_idx = get (cur_idx + 1) in
    (BatVect.set state output_idx program_input)
  | 4 ->
    state (* TODO: update output instead *)
  | _ -> raise (Failure "Invalid opcode")

let run_intcode program_input start_state =
    let rec run_intcode_inner state cur_idx =
        let opcode = BatVect.get state cur_idx in
        if opcode = 99 then
            BatVect.get state 0 (* TODO: return output instead *)
        else
            (* TODO: get list of parameter modes and pass it to apply_op *)
            let next_state = apply_op opcode program_input state cur_idx in
            let param_count = parameter_count opcode in
            run_intcode_inner next_state (cur_idx + param_count) in
    run_intcode_inner start_state 0

let () =
    let program_input = 1 in (* as per spec *)
    let start_state =
        let file_input = In_channel.read_lines "../5.txt" |> List.hd_exn in
        List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
    let output = run_intcode program_input start_state in
    print_endline (string_of_int output)
