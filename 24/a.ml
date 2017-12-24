open! Core

let sort_two (a, b) = if a > b then (b, a) else (a, b)

let rec dfs parts start end_ =
  let best = ref 0 in
  for next = 0 to end_ do
    match Hashtbl.find parts (sort_two (start, next)) with
    | None | Some 0 -> ()
    | Some _ ->
      Hashtbl.decr parts (sort_two (start, next));
      let r = dfs parts next end_ in
      Hashtbl.incr parts (sort_two (start, next));
      let s = r + start + next in
      if s > !best
      then (best := s)
  done;
  !best
;;

let () =
  let input =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      match String.split line ~on:'/' with
      | [ a; b ] -> (Int.of_string a, Int.of_string b)
      | _ -> assert false)
  in
  let parts = Hashtbl.Poly.create () in
  let end_ = ref 0 in
  List.iter input ~f:(fun (a, b) ->
    Hashtbl.incr parts (sort_two (a, b));
    if a > !end_ then (end_ := a);
    if b > !end_ then (end_ := b));
  dfs parts 0 !end_
  |> printf "%d\n"
;;
