open! Core

let sort_two (a, b) = if a > b then b, a else a, b
let cmp = [%compare: int * int]

let rec dfs parts start end_ =
  let best = ref (0, 0) in
  for next = 0 to end_ do
    match Hashtbl.find parts (sort_two (start, next)) with
    | None | Some 0 -> ()
    | Some _ ->
      Hashtbl.decr parts (sort_two (start, next));
      let l, s = dfs parts next end_ in
      Hashtbl.incr parts (sort_two (start, next));
      if cmp (l + 1, s + start + next) !best > 0 then best := l + 1, s + start + next
  done;
  !best
;;

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      match String.split line ~on:'/' with
      | [ a; b ] -> Int.of_string a, Int.of_string b
      | _ -> assert false)
  in
  let parts = Hashtbl.Poly.create () in
  let end_ = ref 0 in
  List.iter input ~f:(fun (a, b) ->
    Hashtbl.incr parts (sort_two (a, b));
    if a > !end_ then end_ := a;
    if b > !end_ then end_ := b);
  dfs parts 0 !end_ |> snd |> printf "%d\n"
;;
