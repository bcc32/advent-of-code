open! Core

let () =
  In_channel.with_file Sys.argv.(1) ~f:(fun file ->
    In_channel.input_lines file
    |> List.sum (module Int) ~f:(fun row ->
      let nums =
        String.split row ~on:'\t'
        |> List.map ~f:Int.of_string
      in
      let cmp = Int.compare in
      let min, max = List.min_elt ~cmp nums, List.max_elt ~cmp nums in
      uw max - uw min)
    |> printf "%d\n")
