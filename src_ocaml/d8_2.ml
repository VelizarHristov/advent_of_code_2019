open Core
open Printf

let () =
    let numbers = In_channel.read_lines "../8.txt"
      |> List.hd_exn |> String.to_list |> List.map ~f:Char.get_digit_exn in
    let layers = List.chunks_of numbers ~length:(25 * 6) in
    let visible_pixels = List.map ~f:(fun i -> 
      let ith = List.map ~f:(fun ls -> List.nth_exn ls i) layers in
      List.find ith ~f:(fun d -> d <> 2) |> Option.value_exn
    ) (List.range 0 (25 * 6)) in
    let visible_pixels_strings = List.map ~f:string_of_int visible_pixels in
    let rows = List.chunks_of visible_pixels_strings 25 in
    let as_string = String.concat ~sep:"\n" (List.map ~f:String.concat rows) in
    print_endline (as_string)
