open! Core
open! Async
open! Import
open Intcode
open Program.Infix

let a () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  program.$(1) <- 12;
  program.$(2) <- 2;
  Program.Sync.run_without_input_exn program ~f:ignore;
  printf "%d\n" program.$(0);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 12490719 |}]
;;

let try_ (program : Program.t) ~noun ~verb =
  program.$(1) <- noun;
  program.$(2) <- verb;
  Program.Sync.run_without_input_exn program ~f:ignore;
  program.$(0)
;;

let b () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  let snapshot = Program.snapshot program in
  for noun = 0 to 99 do
    for verb = 0 to 99 do
      Program.restore program ~from:snapshot;
      match try_ program ~noun ~verb with
      | 19690720 -> printf "%d\n" ((100 * noun) + verb)
      | _ -> ()
    done
  done;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 2003 |}]
;;
