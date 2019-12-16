open Batteries
open Core
open Printf

let tuple_to_str (x, y) = "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let blockers x y =
    let cd = abs (gcd x y) in
    List.range 1 cd |> List.map ~f:(fun mult -> (x / cd * mult, y / cd * mult))
let is_visible x y asteroid_exists_relative =
    not (List.exists ~f:(fun (x, y) ->
        asteroid_exists_relative x y
    ) (blockers x y))


let next_values (x, y) = 
    let cd = abs (gcd x y) in
    (x + x / cd, y + y / cd)
let rec find_in_boundaries x y x2 y2 x_grid_size y_grid_size has_asteroid =
    let posx = x + x2 in
    let posy = y + y2 in
    if posx < x_grid_size && posy < y_grid_size && posx >= 0 && posy >= 0 then
        if has_asteroid posx posy then
            Some (x2, y2)
        else
            let (next_x2, next_y2) = next_values (x2, y2) in
            find_in_boundaries x y next_x2 next_y2 x_grid_size y_grid_size has_asteroid
    else
        None

let rec blast_order x y x_grid_size y_grid_size has_asteroid ratios =
    if List.is_empty ratios then
        []
    else
        let matching = ratios
          |> List.map ~f:(fun (x2, y2) -> find_in_boundaries x y x2 y2 x_grid_size y_grid_size has_asteroid)
          |> List.filter ~f:Option.is_some
          |> List.map ~f:(fun e -> Option.value_exn e) in
        let blasted = List.map ~f:(fun (x2, y2) -> (x + x2, y + y2)) matching in
        (* List.iter ratios_in_boundaries ~f:(fun xy -> tuple_to_str xy |> print_endline); *)
        let next = List.map ~f:next_values matching in
        blasted :: (blast_order x y x_grid_size y_grid_size has_asteroid next)
let get_visible_asteroid_count x y x_grid_size y_grid_size has_asteroid ratios = List.length (List.hd_exn (blast_order x y x_grid_size y_grid_size has_asteroid ratios))
let get_blast_order x y x_grid_size y_grid_size has_asteroid ratios = List.concat (blast_order x y x_grid_size y_grid_size has_asteroid ratios)

let () =
    let lines = In_channel.read_lines "../10.txt" in
    let grid =
        List.map ~f:(fun line ->
            String.to_list line |>
            List.map ~f:(Char.equal '#') |>
            BatVect.of_list
        ) lines |> BatVect.of_list in
    let x_grid_size = BatVect.length (BatVect.get grid 0) in
    let y_grid_size = BatVect.length grid in
    let get x y = (BatVect.get (BatVect.get grid y) x) in
    let grid_positions = List.map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) (List.range 0 y_grid_size)) (List.range 0 x_grid_size) |> List.concat in
    let to_ratio (x, y) = if y = 0 then Float.max_value else ((float_of_int x) /. (float_of_int y)) in
    let quadrant_ratios = grid_positions
        |> List.filter ~f:(fun (x, y) -> gcd x y = 1)
        |> List.sort ~compare:(fun a b -> Float.compare (to_ratio a) (to_ratio b))
        |> List.map ~f:(fun (x, y) -> (x, -y)) in
    let ratios = List.concat([
        quadrant_ratios;
        List.map ~f:(fun (x, y) -> (x, -y)) quadrant_ratios |> List.filter ~f:(fun (_, y) -> y <> 0) |> List.rev;
        List.map ~f:(fun (x, y) -> (-x, -y)) quadrant_ratios |> List.filter ~f:(fun (x, y) -> x <> 0 && y <> 0);
        List.map ~f:(fun (x, y) -> (-x, y)) quadrant_ratios |> List.filter ~f:(fun (x, _) -> x <> 0) |> List.rev]) in
    let asteroid_positions = List.filter ~f:(fun (x, y) -> get x y) grid_positions |> BatVect.of_list in
    let visible_asteroids_counts = BatVect.map (fun (x, y) ->
        let len = get_visible_asteroid_count x y x_grid_size y_grid_size get ratios in
        (x, y, len)
    ) asteroid_positions in

    let (x, y, _) = List.max_elt (BatVect.to_list visible_asteroids_counts) ~compare:(fun a b -> compare (Tuple3.get3 a) (Tuple3.get3 b)) |> Option.value_exn in
    let order = get_blast_order x y x_grid_size y_grid_size get ratios in

    print_endline ("base: (" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")");
    let i = ref 1 in
    List.iter order ~f:(fun (xy2) ->
        print_endline ((string_of_int !i) ^ ": " ^ (tuple_to_str xy2));
        i := !i + 1
    )
