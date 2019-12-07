open! Core
open! Async
open! Import
open Intcode

let a () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  let output =
    Pipe.create_reader ~close_on_exception:true (fun output ->
      Program.run program ~input:(Pipe.of_list [ 1 ]) ~output)
  in
  Pipe.iter_without_pushback output ~f:(printf "%d\n")
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {|
    0
    0
    0
    0
    0
    0
    0
    0
    0
    15426686 |}]
;;

let b () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  let output =
    Pipe.create_reader ~close_on_exception:true (fun output ->
      Program.run program ~input:(Pipe.of_list [ 5 ]) ~output)
  in
  Pipe.iter_without_pushback output ~f:(printf "%d\n")
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 11430197 |}]
;;
