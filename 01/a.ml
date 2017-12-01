open! Core

let count_matches s =
  let count = ref 0 in
  let check i j =
    if s.[i] = s.[j]
    then (count := !count + (int_of_char s.[i] - int_of_char '0'))
  in
  check 0 (String.length s - 1);
  for i = 1 to String.length s - 1 do
    check (i - 1) i
  done;
  !count
;;

let () =
  In_channel.with_file Sys.argv.(1) ~f:(fun file ->
    file
    |> In_channel.input_all
    |> String.strip
    |> count_matches
    |> Printf.printf "%d\n")
;;
