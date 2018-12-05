open! Core
open! Async
open! Import

let main () =
  printf "output\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| output |}]
;;
