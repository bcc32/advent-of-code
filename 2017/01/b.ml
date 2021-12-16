open! Core

let count_matches s =
  let count = ref 0 in
  let check i j =
    if Char.( = ) s.[i] s.[j] then count := !count + (int_of_char s.[i] - int_of_char '0')
  in
  let length = String.length s in
  let half = length / 2 in
  for i = 0 to length - 1 do
    check i ((i + half) % length)
  done;
  !count
;;

let () =
  In_channel.with_file
    (Sys.get_argv ()).(1)
    ~f:(fun file ->
      file
      |> In_channel.input_all
      |> String.strip
      |> count_matches
      |> Printf.printf "%d\n")
;;
