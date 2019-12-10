open Core
open Printf

type change = { direction: char; magnitude: int }
let change_of_line str = 
    let magnitude = String.slice str 1 (String.length str) |> int_of_string in
    { direction = str.[0]; magnitude = magnitude }

let changes_of_line line = List.map ~f:change_of_line (String.split line ~on:',')

let inc_vec start direction =
    let (x, y) = start in
    match direction with
        | 'R' -> (x + 1, y)
        | 'L' -> (x - 1, y)
        | 'U' -> (x, y + 1)
        | _   -> (x, y - 1)

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

let rec movement_path start steps change =
    if change.magnitude = 0 then
        (start, steps, Map.empty (module CmpIntTuple))
    else
        let current_pos = (inc_vec start change.direction) in
        let (final_pos, final_steps, remaining_path) = (movement_path current_pos (steps + 1) { change with magnitude = change.magnitude - 1 }) in
        (final_pos, final_steps, Map.add_exn remaining_path ~key:current_pos ~data:steps)

let () =
    let lines = In_channel.read_lines "../3.txt" in
    let change_seqs = List.map ~f:changes_of_line lines in
    let path_sets_with_steps = List.map ~f:(fun changes ->
        List.fold_left ~init:((0, 0), 1, Map.empty (module CmpIntTuple)) ~f:(fun (pos, steps, acc_path) change ->
            let (next_pos, next_steps, next_path) = movement_path pos steps change in
            let new_path = Map.merge_skewed next_path acc_path ~combine:(fun ~key:_ steps1 _ -> steps1) in
            (next_pos, next_steps, new_path)
        ) changes
    ) change_seqs |> List.map ~f:Tuple3.get3 in
    let positions1 :: positions2 :: [] = List.map ~f:(fun path_map ->
      (Map.keys path_map) |> Set.of_list (module CmpIntTuple)
    ) path_sets_with_steps in
    let intersecting_positions = Set.inter positions1 positions2 in
    let intersection_steps = Set.map (module Int) ~f:(fun pos ->
      let steps1 :: steps2 :: [] = List.map ~f:(fun path_set -> Map.find_exn path_set pos) path_sets_with_steps in
      steps1 + steps2
    ) intersecting_positions in
    let min_steps = Set.min_elt_exn intersection_steps in
    print_endline (string_of_int (min_steps))
