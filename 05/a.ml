open! Core

let simulate steps =
  let n = Array.length steps in
  let rec loop index count =
    if index < 0 || index >= n
    then count
    else (
      let step = steps.( index ) in
      steps.( index ) <- steps.( index ) + 1;
      loop (index + step) (count + 1))
  in
  loop 0 0
;;

let () =
  let steps =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:(fun file ->
      In_channel.input_lines file
      |> List.to_array
      |> Array.map ~f:Int.of_string)
  in
  simulate steps
  |> printf "%d\n"
;;
