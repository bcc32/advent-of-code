open! Core
open! Async
open! Import

let main () =
  let%bind claims =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Claim.of_string |> Pipe.to_list)
  in
  let claims_by_square = Hashtbl.create (module Tuple.Hashable_t (Int) (Int)) in
  List.iter claims ~f:(Claim.iter ~f:(fun x y -> Hashtbl.incr claims_by_square (x, y)));
  claims_by_square |> Hashtbl.count ~f:(fun x -> x >= 2) |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 109785 |}];
  return ()
;;
