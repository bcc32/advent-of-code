open! Core
open! Async
open! Import

let main () =
  let%bind points =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Point.of_string |> Pipe.to_list)
  in
  let secs =
    Points.to_sequence points
    |> Sequence.fold_until
         ~init:(Int.max_value, -1)
         ~f:(fun (last_area, secs) (_, area) ->
           if area > last_area then Stop secs else Continue (area, secs + 1))
         ~finish:(fun _ -> assert false)
  in
  printf "%d\n" secs;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 10942 |}];
  return ()
;;
