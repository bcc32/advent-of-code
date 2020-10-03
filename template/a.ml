open! Core
open! Async
open! Import

let%expect_test _ =
  print_endline "hello";
  [%expect {| hello |}]
;;
