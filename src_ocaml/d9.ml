open Batteries
open Core
open Printf

let get_code code i = if i > ((BatVect.length code) - 1) then Big_int.of_int 0 else BatVect.get code i
let rec write_code code i new_elem =
  let length_diff = i + 1 - (BatVect.length code) in
  if length_diff < 0  then
    BatVect.set code i new_elem
  else
    write_code (BatVect.append (Big_int.of_int 0) code) i new_elem

let print_code code =
  BatVect.map Big_int.to_string code |>
  BatVect.reduce (fun a b -> a ^ "," ^ b) |>
  print_endline

type machine_state = {
  code: Big_int.t BatVect.t;
  idx: int;
  rel_base: int;
  inputs: int list;
  outputs: Big_int.t list
}

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

let get_param_idx_from state param_modes i =
  let {code; idx; rel_base; _} = state in
  match param_modes (i - 1) with
    | '0' -> get_code code (idx + i) |> Big_int.to_int
    | '1' -> raise (Failure "get_param_idx for opcode 1")
    | '2' -> Big_int.to_int (get_code code (idx + i)) + rel_base
    | _ -> raise (Failure "Invalid param code")

let get_param_from state param_modes i =
  let {code; idx; rel_base; _} = state in
  match param_modes (i - 1) with
    | '0' -> get_code code (get_code code (idx + i) |> Big_int.to_int)
    | '1' -> get_code code (idx + i)
    | '2' -> get_code code (Big_int.to_int (get_code code (idx + i)) + rel_base)
    | _ -> raise (Failure "Invalid param code")

let apply_op opcode get_param get_param_idx state =
  let {code; idx; inputs; _} = state in
  match opcode with
  | 1 ->
    let output = Big_int.(+) (get_param 1) (get_param 2) in
    let new_code = (write_code code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 2 ->
    let output = Big_int.mul (get_param 1) (get_param 2) in
    let new_code = (write_code code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 3 ->
    let next_input::remaining_inputs = inputs in
    let new_code = (write_code code (get_param_idx 1) (Big_int.of_int next_input)) in
    {state with idx = idx + 2; code = new_code; inputs = remaining_inputs}
  | 4 -> {state with idx = idx + 2; outputs = state.outputs @ [get_param 1]}
  | 5 -> {state with idx = if not (Big_int.eq_big_int (get_param 1) (Big_int.of_int 0)) then (get_param 2) |> Big_int.to_int else idx + 3}
  | 6 -> {state with idx = if Big_int.eq_big_int (get_param 1) (Big_int.of_int 0) then (get_param 2) |> Big_int.to_int else idx + 3}
  | 7 ->
    let output = if Big_int.lt_big_int (get_param 1) (get_param 2) then Big_int.of_int 1 else Big_int.of_int 0 in
    let new_code = (write_code code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 8 ->
    let output = if Big_int.eq_big_int (get_param 1) (get_param 2) then Big_int.of_int 1 else Big_int.of_int 0 in
    let new_code = (write_code code (get_param_idx 3) output) in
    {state with idx = idx + 4; code = new_code}
  | 9 -> {state with idx = idx + 2; rel_base = state.rel_base + (Big_int.to_int (get_param 1))}
  | _ -> raise (Failure "Invalid opcode")

let rec run_intcode state =
  let {code; idx; _} = state in
  let raw_opcode = BatVect.get code idx |> Big_int.to_int in
  let (opcode, param_modes) = parse_opcode raw_opcode in
  if opcode = 99 then
    state.outputs
  else
    let get_param = get_param_from state param_modes in
    let get_param_idx = get_param_idx_from state param_modes in
    let next_state = apply_op opcode get_param get_param_idx state in
    run_intcode next_state

let () =
    let start_state =
      let file_input = In_channel.read_lines "../9.txt" |> List.hd_exn in
      let program_input = 1 in (* 1 for part 1, 2 for part 2 *)
      let start_code = List.map ~f:Big_int.of_string (String.split file_input ~on:',') |> BatVect.of_list in
      { code = start_code; idx = 0; rel_base = 0; inputs = [program_input]; outputs = [] } in
    let outputs = run_intcode start_state in
    let outputs_str = List.map ~f:Big_int.to_string outputs in
    print_endline (String.concat outputs_str);
    print_endline (String.concat ~sep:"," outputs_str)
