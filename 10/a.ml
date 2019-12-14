open! Core

let () =
  let lengths =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:(fun file ->
      In_channel.input_line_exn file
      |> String.split ~on:','
      |> List.map ~f:Int.of_string)
  in
  let list = Array.init 256 ~f:Fn.id in
  let pos = ref 0 in
  let skip = ref 0 in
  List.iter lengths ~f:(fun len ->
    for i = 0 to len / 2 - 1 do
      let left = !pos + i in
      let right = !pos + (len - i - 1) in
      let left, right = left % 256, right % 256 in
      let t = list.( left ) in
      list.( left ) <- list.( right );
      list.( right ) <- t
    done;
    pos := !pos + len + !skip;
    incr skip);
  list.( 0 ) * list.( 1 )
  |> printf "%d\n"
;;
