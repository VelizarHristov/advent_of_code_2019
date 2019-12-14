open Core
open Printf

let count_digit digit = List.count ~f:(fun e -> e = digit)

let () =
    let numbers = In_channel.read_lines "../8.txt"
      |> List.hd_exn |> String.to_list |> List.map ~f:Char.get_digit_exn in
    let layers = List.chunks_of numbers ~length:(25 * 6) in
    let min_zeroes_layer = List.max_elt layers ~compare:(fun a b ->
      compare ((count_digit 0) b) ((count_digit 0) a)
    ) |> Option.value_exn in
    let prod = ((count_digit 1) min_zeroes_layer) * ((count_digit 2) min_zeroes_layer) in
    print_endline (string_of_int prod)
