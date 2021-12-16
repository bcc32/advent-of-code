open! Core
open! Async
open! Import

let input () =
  let%map lines = Reader.file_lines "input" in
  lines
;;

let a () =
  let%bind _input = input () in
  printf "output\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| output |}]
;;

let b () =
  let%bind _input = input () in
  printf "output\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| output |}]
;;
