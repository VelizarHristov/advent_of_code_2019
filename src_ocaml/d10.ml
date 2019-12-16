open Batteries
open Core
open Printf

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let blockers x y =
    let cd = abs (gcd x y) in
    List.range 1 cd |> List.map ~f:(fun mult -> (x / cd * mult, y / cd * mult))
let is_visible x y asteroid_exists_relative =
    not (List.exists ~f:(fun (x, y) ->
        asteroid_exists_relative x y
    ) (blockers x y))

let () =
    let lines = In_channel.read_lines "../10.txt" in
    let grid =
        List.map ~f:(fun line ->
            String.to_list line |>
            List.map ~f:(Char.equal '#') |>
            BatVect.of_list
        ) lines |> BatVect.of_list in
    let grid_size = BatVect.length grid in
    let get x y = (BatVect.get (BatVect.get grid y) x) in
    let asteroid_positions = List.map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) (List.range 0 grid_size)) (List.range 0 grid_size) |>
        List.concat |>
        List.filter ~f:(fun (x, y) -> get x y) |>
        BatVect.of_list in
    let visible_asteroids_counts = BatVect.map (fun (x, y) ->
        let relative_positions = BatVect.map (fun (x2, y2) -> (x2 - x, y2 - y)) asteroid_positions in
        let asteroid_exists_relative x2 y2 = get (x + x2) (y + y2) in
        let visible_asteroids = BatVect.filter (fun (x2, y2) ->
            if x2 = 0 && y2 = 0 then false else is_visible x2 y2 asteroid_exists_relative
        ) relative_positions in
        BatVect.length visible_asteroids
    ) asteroid_positions in

    let max_output = (List.reduce_exn ~f:max (BatVect.to_list visible_asteroids_counts)) in
    print_endline (string_of_int max_output)
