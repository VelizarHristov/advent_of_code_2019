open Batteries
open Core
open Printf

let print_state state = BatVect.fold (fun a b -> a ^ "," ^ (string_of_int b)) "" state |> print_endline

type machine_state = { numbers: int BatVect; inputs: int list; idx: int }

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

let apply_op opcode param_modes program_inputs state idx =
  let get = BatVect.get state in
  let get_input i = match param_modes i with
    | '0' -> get (get (idx + i + 1))
    | '1' -> get (idx + i + 1)
    | _ -> raise (Failure "Invalid param code") in
  match opcode with
  | 1 ->
    let output_idx = get (idx + 3) in
    let output = (get_input 0) + (get_input 1) in
    ((BatVect.set state output_idx output), false, None, idx + 4)
  | 2 ->
    let output_idx = get (idx + 3) in
    let output = (get_input 0) * (get_input 1) in
    ((BatVect.set state output_idx output), false, None, idx + 4)
  | 3 ->
    let output_idx = get (idx + 1) in
    ((BatVect.set state output_idx (List.hd_exn program_inputs)), true, None, idx + 2)
  | 4 -> (state, false, Some (get_input 0), idx + 2)
  | 5 ->
    let next_idx = if (get_input 0) <> 0 then (get_input 1) else idx + 3 in
    (state, false, None, next_idx)
  | 6 ->
    let next_idx = if (get_input 0) = 0 then (get_input 1) else idx + 3 in
    (state, false, None, next_idx)
  | 7 ->
    let output_idx = get (idx + 3) in
    let output = if (get_input 0 < get_input 1) then 1 else 0 in
    ((BatVect.set state output_idx output), false, None, idx + 4)
  | 8 ->
    let output_idx = get (idx + 3) in
    let output = if (get_input 0 = get_input 1) then 1 else 0 in
    ((BatVect.set state output_idx output), false, None, idx + 4)
  | _ -> raise (Failure "Invalid opcode")

let rec run_intcode_inner state program_inputs cur_idx =
    let raw_opcode = BatVect.get state cur_idx in
    let (opcode, param_modes) = parse_opcode raw_opcode in
    if opcode = 99 then
      None
    else
      let (next_state, input_was_read, program_output, next_idx) = apply_op opcode param_modes program_inputs state cur_idx in
      let next_program_inputs = if input_was_read then List.tl_exn program_inputs else program_inputs in
      (next_state, next_program_inputs, program_output, next_idx) in

let drop_nth ls n = List.take ls n @ List.drop ls (n + 1)
let selections ls = List.map ~f:(fun i -> (List.nth_exn ls i, drop_nth ls i)) (List.range 0 (List.length ls))
let rec permutations ls = if List.is_empty ls then [[]] else
    List.map ~f:(fun (first, rest) ->
        List.map ~f:(fun perm -> first :: perm) (permutations rest)
    ) (selections ls) |> List.concat

let rec repeat n e = if n = 0 then [] else e :: (repeat (n - 1) e)

(* let rec run_cyclic states last_output phase_settings =
  let iter = List.fold_until ~init:(states, last_output) ~f:(fun (cur_state::states, input_1) phase_setting ->
    match run_intcode cur_state (phase_setting @ [input_1]) with
      | (Some output, next_state) -> Continue (states @ [next_state], output)
      | (None, _) -> Stop None
    ) ~finish:(fun output_and_next_states -> Some output_and_next_states) phase_settings in
  match iter with
    | Some (next_states, next_output) -> run_cyclic next_states next_output (List.map ~f:(fun _ -> []) phase_settings)
    | None -> last_output *)

let rec run_cyclic states inputs =
  let iter = List.fold_until ~init:(states, inputs) ~f:(fun (cur_state::states, input::next_inputs) _ ->
    match run_intcode cur_state [input] with
      | (Some output, next_state) -> Continue (states @ [next_state], next_inputs @ [output])
      | (None, _) -> Stop None
    ) ~finish:(fun output_and_next_states -> Some output_and_next_states) (List.range 0 5) in
  match iter with
    | Some (next_states, next_inputs) -> run_cyclic next_states next_inputs
    | None -> List.last_exn inputs

let () =
    let start_state =
      let file_input = In_channel.read_lines "../7.txt" |> List.hd_exn in
      List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
    let start_states = repeat 5 start_state in
    let phase_setting_permutations = permutations (List.range 5 10) in
    (* Assumes that the max permutation's output is more than 0? *)
    let all_outputs = List.map ~f:(fun inputs -> run_cyclic start_states (inputs @ [0])) phase_setting_permutations in
    let max_output = (List.reduce_exn ~f:(Stdlib.max) all_outputs) in
    print_endline (string_of_int max_output)
