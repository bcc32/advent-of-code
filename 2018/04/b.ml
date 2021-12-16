open! Core
open! Async
open! Import

let main () =
  let%bind events =
    Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list)
    (* times can be compared lexicographically *)
    >>| List.sort ~compare:String.compare
    >>| List.map ~f:Event.of_string
  in
  let guard_minute_count = Hashtbl.create (module Tuple.Hashable_t (Int) (Int)) in
  events
  |> Event.analyze
  |> List.iter ~f:(fun (guard_id, (x, y)) ->
    for m = x to y - 1 do
      Hashtbl.incr guard_minute_count (guard_id, m)
    done);
  let guard, minute =
    Hashtbl.to_alist guard_minute_count
    |> List.max_elt ~compare:(Comparable.lift [%compare: int] ~f:snd)
    |> Option.value_exn
    |> fst
  in
  printf "%d\n" (guard * minute);
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 56901 |}]
;;
