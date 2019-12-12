open Batteries
open Core
open Printf

let print_state state = BatVect.fold (fun a b -> a ^ "," ^ (string_of_int b)) "" state |> print_endline

(*
  output: (int, int -> int) - the first is the opcode,
  the second returns the parameter mode of the ith parameter, or 0 if i is out of boundaries
*)
let parse_opcode raw_opcode =
  let chars = String.to_list (string_of_int raw_opcode) in
  let opcode =
    let opcode_chars = List.drop chars ((List.length chars) - 2) in
    int_of_string (String.of_char_list opcode_chars) in
  let param_mode_chars = List.take chars ((List.length chars) - 2) |> List.rev in
  let get_param = fun i ->
    let retrieved = List.nth param_mode_chars i in
    Option.value retrieved ~default:'0' in
  (opcode, get_param)

(* counts the opcode as a parameter *)
let parameter_count opcode = match opcode with
  | 1 -> 4
  | 2 -> 4
  | 3 -> 2
  | 4 -> 2
  | _ -> raise (Failure "Invalid opcode")

let apply_op opcode param_modes program_input program_output state idx =
  let get = BatVect.get state in
  let get_input i idx = match param_modes i with
    | '0' -> get (get idx)
    | '1' -> get idx
    | _ -> raise (Failure "Invalid param code") in
  match opcode with
  | 1 ->
    let input_1 = get_input 0 (idx + 1) in
    let input_2 = get_input 1 (idx + 2) in
    let output_idx = get (idx + 3) in
    let output = input_1 + input_2 in
    ((BatVect.set state output_idx output), program_output)
  | 2 ->
    let input_1 = get_input 0 (idx + 1) in
    let input_2 = get_input 1 (idx + 2) in
    let output_idx = get (idx + 3) in
    let output = input_1 * input_2 in
    ((BatVect.set state output_idx output), program_output)
  | 3 ->
    let output_idx = get (idx + 1) in
    ((BatVect.set state output_idx program_input), program_output)
  | 4 -> (state, get_input 0 (idx + 1))
  | _ -> raise (Failure "Invalid opcode")

let run_intcode program_input start_state =
  let rec run_intcode_inner state program_output cur_idx =
    let raw_opcode = BatVect.get state cur_idx in
    let (opcode, param_modes) = parse_opcode raw_opcode in
    if opcode = 99 then
      program_output
    else
      let (next_state, next_program_output) = apply_op opcode param_modes program_input program_output state cur_idx in
      let param_count = parameter_count opcode in
      run_intcode_inner next_state next_program_output (cur_idx + param_count) in
  run_intcode_inner start_state (-1) 0

let () =
    let program_input = 1 in (* as per spec *)
    let start_state =
      let file_input = In_channel.read_lines "../5.txt" |> List.hd_exn in
      List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
    let output = run_intcode program_input start_state in
    print_endline (string_of_int output)
