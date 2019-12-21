open Batteries
open Core
open Printf
open Intcode

module CmpIntTuple = struct
  module IntTuple = struct
    type t = int * int
    let compare (x0, y0) (x1, y1) =
      match Pervasives.compare x0 x1 with
        0 -> Pervasives.compare y0 y1
      | c -> c

    let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
    let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  end
  include IntTuple
  include Comparable.Make(IntTuple)
end

let idx_exn ls e = List.findi ~f:(fun _ e2 -> Char.equal e e2) ls |> Option.value_exn |> Tuple2.get1
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let directions = ['U'; 'R'; 'D'; 'L']
let toggle_direction direction i =
    let current_idx = idx_exn directions direction in
    let new_idx = modulo (current_idx + i) (List.length directions) in
    List.nth_exn directions new_idx
let move_in_direction (x, y) dir = match dir with
    | 'U' -> (x, y + 1)
    | 'R' -> (x + 1, y)
    | 'D' -> (x, y - 1)
    | 'L' -> (x - 1, y)

let rec run_intcode state = match Intcode.run_intcode_once state with
  | Some next_state when List.length (next_state.outputs) >= 2 -> Some next_state
  | Some next_state -> run_intcode next_state
  | None -> None

let rec run_robot_cleaning_inner state current_pos current_direction colors =
  let (x, y) = current_pos in
  match run_intcode state with
    | Some res_state ->
      let paint_color_raw :: turn_direction_raw :: [] = res_state.outputs in
      let paint_color = Big_int.to_int paint_color_raw in
      let turn_direction = if (Big_int.to_int turn_direction_raw) = 0 then -1 else 1 in

      let new_colors = Map.set colors ~key:current_pos ~data:paint_color in
      let new_direction = toggle_direction current_direction turn_direction in
      let new_pos = move_in_direction current_pos new_direction in

      let next_input = Map.find new_colors new_pos |> Option.value ~default:0 in
      let next_state = { res_state with inputs = [next_input]; outputs = [] } in
      run_robot_cleaning_inner next_state new_pos new_direction new_colors
    | None -> colors
let run_robot_cleaning state = run_robot_cleaning_inner state (0, 0) 'U' (Map.empty (module CmpIntTuple))

let () =
    let start_state =
      let file_input = In_channel.read_lines "../11.txt" |> List.hd_exn in
      let start_code = List.map ~f:Big_int.of_string (String.split file_input ~on:',') |> BatVect.of_list in
      { code = start_code; idx = 0; rel_base = 0; inputs = [0]; outputs = [] } in

    let painted_colors = run_robot_cleaning start_state in
    let num_painted_tiles = Map.keys painted_colors |> List.length in
    print_endline (string_of_int num_painted_tiles)
