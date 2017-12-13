open! Core

let () =
  let layers =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      let index = String.index_exn line ':' in
      let depth = line |> String.subo ~len:index |> Int.of_string in
      let range = line |> String.subo ~pos:(index + 2) |> Int.of_string in
      (depth, range))
    |> Int.Map.of_alist_exn
  in
  let rec loop delay =
    let is_passable =
      Map.for_alli layers ~f:(fun ~key:depth ~data:range ->
        let time = delay + depth in
        time % (2 * (range - 1)) <> 0)
    in
    if is_passable
    then delay
    else (loop (delay + 1))
  in
  loop 0 |> printf "%d\n"
;;
