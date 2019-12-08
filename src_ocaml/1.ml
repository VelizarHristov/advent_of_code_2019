open Core
open Printf

let fuel_per_module module_weight = module_weight / 3 - 2

let () =
    let lines = In_channel.read_lines "../1.txt" in
    let module_weights = List.map ~f:(Fn.compose fuel_per_module int_of_string) lines in
    let total = (List.reduce_exn ~f:(+) module_weights) in
    print_endline (string_of_int total)
