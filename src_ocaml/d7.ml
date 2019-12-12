open Core
open Printf

let drop_nth ls n = List.take ls n @ List.drop ls (n + 1) in
let selections ls = List.map ~f:(fun i -> (List.nth_exn ls i, drop_nth ls i)) (List.range 0 (List.length ls)) in
let rec permutations ls = if List.is_empty ls then [[]] else
    List.map ~f:(fun (first, rest) ->
        List.map ~f:(fun perm -> first :: perm) (permutations rest)
    ) (selections ls) |> List.concat

let () =
    print_endline "TODO"
