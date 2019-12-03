open! Core
open! Async
open! Import

let a () =
  let%bind _lines = Reader.file_lines "input" in
  printf "output\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| output |}]
;;

let b () = a ()

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| output |}]
;;
