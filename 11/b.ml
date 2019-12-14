open! Core

let distance x y =
  let x = ref x in
  let y = ref y in
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
  !steps
;;

let () =
  let steps =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:(fun file ->
      In_channel.input_line_exn file
      |> String.split ~on:',')
  in
  let x = ref 0 in
  let y = ref 0 in
  let max = ref 0 in
  List.iter steps ~f:(fun dir ->
    (match dir with
    | "n" -> incr y
    | "s" -> decr y
    | "ne" -> incr x
    | "sw" -> decr x
    | "nw" -> decr x; incr y
    | "se" -> incr x; decr y
    | _ -> assert false);
    let d = distance !x !y in
    if d > !max then (max := d));
  printf "%d\n" !max
;;
