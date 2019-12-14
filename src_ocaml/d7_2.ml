open Batteries
open Core
open Printf

let print_code code =
  BatVect.map string_of_int code |>
  BatVect.reduce (fun a b -> a ^ "," ^ b) |>
  print_endline

type machine_state = { code: int BatVect.t; idx: int; inputs: int list; output: int option }

let parse_opcode raw_opcode =
  let chars = String.to_list (string_of_int raw_opcode) in
  let opcode =
    let opcode_chars = List.drop chars ((List.length chars) - 2) in
    int_of_string (String.of_char_list opcode_chars) in
  let param_mode_chars = List.take chars ((List.length chars) - 2) |> List.rev in
  let get_param_mode = fun i ->
    let retrieved = List.nth param_mode_chars i in
    Option.value retrieved ~default:'0' in
  (opcode, get_param_mode)

let get_param_from code param_modes idx i = match param_modes (i - 1) with
    | '0' -> BatVect.get code (BatVect.get code (idx + i))
    | '1' -> BatVect.get code (idx + i)
    | _ -> raise (Failure "Invalid param code")

let apply_op opcode get_param state =
  let {code; idx; inputs; _} = state in
  let get_param_idx i = BatVect.get code (idx + i) in
  match opcode with
  | 1 ->
    let output = (get_param 1) + (get_param 2) in
    let new_code = (BatVect.set code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 2 ->
    let output = (get_param 1) * (get_param 2) in
    let new_code = (BatVect.set code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 3 ->
    let next_input::remaining_inputs = inputs in
    let new_code = (BatVect.set code (get_param_idx 1) next_input) in
    {state with idx = idx + 2; code = new_code; inputs = remaining_inputs}
  | 4 -> {state with idx = idx + 2; output = Some (get_param 1)}
  | 5 -> {state with idx = if (get_param 1) <> 0 then (get_param 2) else idx + 3}
  | 6 -> {state with idx = if (get_param 1) = 0 then (get_param 2) else idx + 3}
  | 7 ->
    let output = if (get_param 1 < get_param 2) then 1 else 0 in
    let new_code = (BatVect.set code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 8 ->
    let output = if (get_param 1 = get_param 2) then 1 else 0 in
    let new_code = (BatVect.set code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | _ -> raise (Failure "Invalid opcode")

let rec run_intcode state =
  let {code; idx; _} = state in
  let raw_opcode = BatVect.get code idx in
  let (opcode, param_modes) = parse_opcode raw_opcode in
  if opcode = 99 then
    state
  else
    let get_param = get_param_from code param_modes idx in
    let next_state = apply_op opcode get_param state in
    if (Option.is_some next_state.output) then
      next_state
    else
      run_intcode next_state

let drop_nth ls n = List.take ls n @ List.drop ls (n + 1)
let selections ls = List.map ~f:(fun i -> (List.nth_exn ls i, drop_nth ls i)) (List.range 0 (List.length ls))
let rec permutations ls = if List.is_empty ls then [[]] else
    List.map ~f:(fun (first, rest) ->
        List.map ~f:(fun perm -> first :: perm) (permutations rest)
    ) (selections ls) |> List.concat

let rec run_cyclic states last_output =
  let (next_states, output) = List.fold_left ~init:([], Some last_output) ~f:(fun (states, prev_output) state ->
    let new_inputs = state.inputs @ (Option.to_list prev_output) in
    let next_state = run_intcode { state with inputs = new_inputs; output = None } in
    (states @ [next_state], next_state.output)
  ) states in
  match output with
    | Some o -> run_cyclic next_states o
    | None -> last_output

let () =
    let start_code =
      let file_input = In_channel.read_lines "../7.txt" |> List.hd_exn in
      List.map ~f:int_of_string (String.split file_input ~on:',') |> BatVect.of_list in
    let phase_setting_permutations = permutations (List.range 5 10) in
    let all_outputs = List.map ~f:(fun inputs ->
      let start_states = List.map ~f:(fun input ->
        { code = start_code; idx = 0; inputs = [input]; output = None }
      ) inputs in
      run_cyclic start_states 0
    ) phase_setting_permutations in
    let max_output = (List.reduce_exn ~f:max all_outputs) in
    print_endline (string_of_int max_output)
