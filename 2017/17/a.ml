open! Core

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_all
    |> String.strip
    |> Int.of_string
  in
  let buf = ref (Array.create 0 ~len:1) in
  let pos = ref 0 in
  for next = 1 to 2017 do
    pos := (!pos + input) % Array.length !buf;
    let buf' = Array.create 0 ~len:(1 + Array.length !buf) in
    Array.blit ~src:!buf ~dst:buf' ~src_pos:0 ~dst_pos:0 ~len:(!pos + 1);
    buf'.(!pos + 1) <- next;
    Array.blito () ~src:!buf ~dst:buf' ~src_pos:(!pos + 1) ~dst_pos:(!pos + 2);
    buf := buf';
    incr pos
  done;
  !buf.(!pos + 1) |> printf "%d\n"
;;
