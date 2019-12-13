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

let apply_op opcode param_modes program_inputs program_output state idx =
  let get = BatVect.get state in
  let get_input i = match param_modes i with
    | '0' -> get (get (idx + i + 1))
    | '1' -> get (idx + i + 1)
    | _ -> raise (Failure "Invalid param code") in
  match opcode with
  | 1 ->
    let output_idx = get (idx + 3) in
    let output = (get_input 0) + (get_input 1) in
    ((BatVect.set state output_idx output), false, program_output, idx + 4)
  | 2 ->
    let output_idx = get (idx + 3) in
    let output = (get_input 0) * (get_input 1) in
    ((BatVect.set state output_idx output), false, program_output, idx + 4)
  | 3 ->
    let output_idx = get (idx + 1) in
    ((BatVect.set state output_idx (List.hd_exn program_inputs)), true, program_output, idx + 2)
  | 4 -> (state, false, get_input 0, idx + 2)
  | 5 ->
    let next_idx = if (get_input 0) <> 0 then (get_input 1) else idx + 3 in
    (state, false, program_output, next_idx)
  | 6 ->
    let next_idx = if (get_input 0) = 0 then (get_input 1) else idx + 3 in
    (state, false, program_output, next_idx)
  | 7 ->
    let output_idx = get (idx + 3) in
    let output = if (get_input 0 < get_input 1) then 1 else 0 in
    ((BatVect.set state output_idx output), false, program_output, idx + 4)
  | 8 ->
    let output_idx = get (idx + 3) in
    let output = if (get_input 0 = get_input 1) then 1 else 0 in
    ((BatVect.set state output_idx output), false, program_output, idx + 4)
  | _ -> raise (Failure "Invalid opcode")

let run_intcode start_state initial_program_inputs =
  let rec run_intcode_inner state program_inputs program_output cur_idx =
    let raw_opcode = BatVect.get state cur_idx in
    let (opcode, param_modes) = parse_opcode raw_opcode in
    if opcode = 99 then
      program_output
    else
      let (next_state, input_was_read, next_program_output, next_idx) = apply_op opcode param_modes program_inputs program_output state cur_idx in
      let next_program_inputs = if input_was_read then List.tl_exn program_inputs else program_inputs in
      run_intcode_inner next_state next_program_inputs next_program_output next_idx in
  run_intcode_inner start_state initial_program_inputs (-1) 0

let drop_nth ls n = List.take ls n @ List.drop ls (n + 1)
let selections ls = List.map ~f:(fun i -> (List.nth_exn ls i, drop_nth ls i)) (List.range 0 (List.length ls))
let rec permutations ls = if List.is_empty ls then [[]] else
    List.map ~f:(fun (first, rest) ->
        List.map ~f:(fun perm -> first :: perm) (permutations rest)
    ) (selections ls) |> List.concat

let () =
    let start_state =
      let file_input = In_channel.read_lines "../7.txt" |> List.hd_exn in
      List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
    let phase_setting_permutations = permutations (List.range 0 5) in
    let all_outputs = List.map ~f:(
        List.fold_left ~init:0 ~f:(fun input_1 phase_setting ->
          run_intcode start_state [phase_setting; input_1])
    ) phase_setting_permutations in
    let max_output = (List.reduce_exn ~f:(Stdlib.max) all_outputs) in
    print_endline (string_of_int max_output)
