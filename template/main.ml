open! Core
open! Async
open! Import

let%expect_test "a" =
  let%bind contents = Reader.file_contents "input.txt" in
  print_string contents;
  let%bind () = [%expect {| example |}] in
  return ()
;;

let%expect_test "b" =
  let%bind contents = Reader.file_contents "input.txt" in
  print_string contents;
  let%bind () = [%expect {| example |}] in
  return ()
;;
