open! Core
open! Async
open! Import

let main () =
  let%bind game = Game.get () in
  let scores = Game.simulate game in
  Array.max_elt scores ~compare:Int.compare |> Option.value_exn |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 402398 |}];
  return ()
;;
