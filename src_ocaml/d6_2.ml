open Core
open Printf

let () =
    let outer_to_inner_orbit =
        let assoc_list = In_channel.read_lines "../6.txt" |> List.map ~f:(fun line ->
            let inner :: outer :: [] = (String.split line ~on:')') in
            (outer, inner)
        ) in
        Map.of_alist_exn (module String) assoc_list in

    let rec ancestors_list orbit = if (String.equal orbit "COM") then [] else orbit :: ancestors_list (Map.find_exn outer_to_inner_orbit orbit) in
    let rec travel_distance (orbit1::orbits1) orbits2 = match List.findi orbits2 ~f:(fun _ x -> String.equal x orbit1) with
        | Some (i, _) -> i - 2
        | None -> 1 + (travel_distance orbits1 orbits2) in
    let dist_to_santa = travel_distance (ancestors_list "YOU") (ancestors_list "SAN") in

    print_endline (string_of_int dist_to_santa)
