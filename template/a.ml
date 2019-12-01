open! Core
open! Async
open! Import

let main () =
  let%bind _lines = Reader.file_lines "input" in
  printf "output\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| output |}]
;;
