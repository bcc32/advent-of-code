open! Core
open! Async
open! Import

let main () =
  let%bind carts = Carts.read () in
  try
    while true do
      Carts.step carts
    done;
    assert false
  with
  | Carts.Collision (row, col) ->
    printf "%d,%d\n" col row;
    return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 118,112 |}]
;;
