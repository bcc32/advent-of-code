open! Core

let () =
  In_channel.with_file
    (Sys.get_argv ()).(1)
    ~f:(fun file ->
      In_channel.input_lines file
      |> List.sum
           (module Int)
           ~f:(fun row ->
             let nums = String.split row ~on:'\t' |> List.map ~f:Int.of_string in
             let compare = Int.compare in
             let min, max = List.min_elt ~compare nums, List.max_elt ~compare nums in
             uw max - uw min)
      |> printf "%d\n")
;;
