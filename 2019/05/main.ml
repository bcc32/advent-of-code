open! Core
open! Async
open! Import
open Intcode

let a () =
  let%bind program = Reader.file_contents "aoc.in" >>| Program.of_string in
  let%tydi { input; output; done_ } = Program.Async.run program in
  Pipe.write_without_pushback input 1;
  let%bind () = Pipe.iter_without_pushback output ~f:(printf "%d\n") in
  done_
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
    15426686 |}];
  return ()
;;

let b () =
  let%bind program = Reader.file_contents "aoc.in" >>| Program.of_string in
  let%tydi { input; output; done_ } = Program.Async.run program in
  Pipe.write_without_pushback input 5;
  let%bind () = Pipe.iter_without_pushback output ~f:(printf "%d\n") in
  done_
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 11430197 |}];
  return ()
;;
