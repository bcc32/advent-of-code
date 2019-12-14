open! Core

let input =
  In_channel.with_file (Sys.get_argv ()).(1) ~f:(fun file ->
    In_channel.input_all file
    |> String.strip
    |> Int.of_string)
;;

let side_length =
  let n =
    sqrt (float (input - 1))
    |> Float.iround_down_exn
  in
  if n % 2 = 0
  then (n - 1)
  else n
;;

let index =
  (input - side_length * side_length)
  % (side_length + 1)
;;

let displacement = Int.abs (index - (side_length + 1) / 2)

let total_distance = (side_length + 1) / 2 + displacement

let () = printf "%d\n" total_distance
