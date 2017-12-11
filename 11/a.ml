open! Core

let () =
  let steps =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_line_exn file
      |> String.split ~on:',')
  in
  let x = ref 0 in
  let y = ref 0 in
  List.iter steps ~f:(function
    | "n" -> incr y
    | "s" -> decr y
    | "ne" -> incr x
    | "sw" -> decr x
    | "nw" -> decr x; incr y
    | "se" -> incr x; decr y
    | _ -> assert false);
  let steps = ref 0 in
  while !x < 0 && !y > 0 do
    incr steps;
    incr x;
    decr y;
  done;
  while !x > 0 && !y < 0 do
    incr steps;
    decr x;
    incr y;
  done;
  steps := !steps + abs !x;
  steps := !steps + abs !y;
  printf "%d\n" !steps
;;
