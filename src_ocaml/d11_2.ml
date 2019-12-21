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
    | 'U' -> (x, y - 1)
    | 'R' -> (x + 1, y)
    | 'D' -> (x, y + 1)
    | 'L' -> (x - 1, y)

let rec run_intcode state = match Intcode.run_intcode_once state with
  | Some next_state when List.length (next_state.outputs) >= 2 -> Some next_state
  | Some next_state -> run_intcode next_state
  | None -> None

let rec run_robot_cleaning_inner state current_pos current_direction colors =
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

let rec print_tiles colors =
  let positions = Map.keys colors in
  let x_values = List.map ~f:Tuple2.get1 positions in
  let y_values = List.map ~f:Tuple2.get2 positions in
  let min_x = List.reduce_exn ~f:min x_values in
  let max_x = List.reduce_exn ~f:max x_values in
  let min_y = List.reduce_exn ~f:min y_values in
  let max_y = List.reduce_exn ~f:max y_values in
  for y = min_y to max_y do
    for x = min_x to max_x do
      let color = Map.find colors (x, y) |> Option.value ~default:0 in
      print_string (string_of_int color)
    done;
    print_string "\n"
  done

let () =
    let start_state =
      let file_input = In_channel.read_lines "../11.txt" |> List.hd_exn in
      let start_code = List.map ~f:Big_int.of_string (String.split file_input ~on:',') |> BatVect.of_list in
      { code = start_code; idx = 0; rel_base = 0; inputs = [1]; outputs = [] } in

    let painted_colors = run_robot_cleaning start_state in
    print_tiles painted_colors
