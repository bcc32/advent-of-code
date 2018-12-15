open! Core
open! Async
open! Import

let main () =
  let%bind carts = Carts.read () in
  with_return (fun { return } ->
    while true do
      Carts.step carts ~on_collision:(fun ~row ~col ->
        printf "%d,%d\n" col row;
        return ())
    done);
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 118,112 |}]
;;
