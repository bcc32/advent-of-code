open! Core
open! Async
open! Import

let main () =
  let%bind game = Game.get () in
  let game = { game with last_marble = game.last_marble * 100 } in
  let scores = Game.simulate game in
  Array.max_elt scores ~compare:Int.compare |> Option.value_exn |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 3426843186 |}]
;;
