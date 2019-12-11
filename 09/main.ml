open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let a () =
  let%bind program = input () in
  match Program.run program with
  | { input; output; done_ } ->
    Pipe.write_without_pushback input 1;
    let%bind () = Pipe.iter_without_pushback output ~f:(printf "%d\n") in
    done_
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2662308295 |}]
;;

let b () =
  let%bind program = input () in
  match Program.run program with
  | { input; output; done_ } ->
    Pipe.write_without_pushback input 2;
    let%bind () = Pipe.iter_without_pushback output ~f:(printf "%d\n") in
    done_
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 63441 |}]
;;
