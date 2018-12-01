open! Core
open! Async
open! Import

let main () =
  printf "output\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| output |}]
;;
