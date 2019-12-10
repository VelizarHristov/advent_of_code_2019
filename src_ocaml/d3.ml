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

let rec movement_path start change =
    if change.magnitude = 0 then
        (start, Set.empty (module CmpIntTuple))
    else
        let current_pos = (inc_vec start change.direction) in
        let (final_pos, remaining_path) = (movement_path current_pos { change with magnitude = change.magnitude - 1 }) in
        (final_pos, Set.add remaining_path current_pos)

let () =
    let lines = In_channel.read_lines "../3.txt" in
    let change_seqs = List.map ~f:changes_of_line lines in
    let path_sets = List.map ~f:(fun changes ->
        List.fold_left ~init:((0, 0), Set.empty (module CmpIntTuple)) ~f:(fun (pos, acc_path) change ->
            let (next_pos, next_path) = movement_path pos change in
            (next_pos, Set.union acc_path next_path)
        ) changes
    ) change_seqs |> List.map ~f:Tuple2.get2 in
    let path1 :: path2 :: [] = path_sets in
    let common_positions = Set.inter path1 path2 in
    let min_manhattan_dist = Set.map (module Int) ~f:(fun (x, y) -> abs(x) + abs(y)) common_positions |> Set.min_elt_exn in
    print_endline (string_of_int (min_manhattan_dist))
