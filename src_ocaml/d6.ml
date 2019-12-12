open Core
open Printf

let () =
    let outer_to_inner_orbit =
        let assoc_list = In_channel.read_lines "../6.txt" |> List.map ~f:(fun line ->
            let inner :: outer :: [] = (String.split line ~on:')') in
            (outer, inner)
        ) in
        Map.of_alist_exn (module String) assoc_list in
    let orbit_to_count = Hashtbl.Poly.create () in
    Hashtbl.add_exn orbit_to_count ~key:"COM" ~data:0; (* As per spec *)
    let rec get_orbit_count orbit = match Hashtbl.find orbit_to_count orbit with
        | Some count -> count
        | None ->
            let inner_orbit = Map.find_exn outer_to_inner_orbit orbit in
            let count = (get_orbit_count inner_orbit) + 1 in
            Hashtbl.add_exn orbit_to_count ~key:orbit ~data:count;
            count in
    let orbit_counts = Map.keys outer_to_inner_orbit |> List.map ~f:get_orbit_count in
    let total = (List.reduce_exn ~f:(+) orbit_counts) in

    print_endline (string_of_int total)
