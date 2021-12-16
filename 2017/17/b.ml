open! Core

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_all
    |> String.strip
    |> Int.of_string
  in
  let pos = ref 0 in
  let last_after_zero = ref 1 in
  for next = 1 to 50_000_000 do
    pos := (!pos + input) % next;
    if !pos = 0 then last_after_zero := next;
    incr pos
  done;
  !last_after_zero |> printf "%d\n"
;;
