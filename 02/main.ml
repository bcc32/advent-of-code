open! Core
open! Async
open! Import
open Intcode

let a () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  program.(1) <- 12;
  program.(2) <- 2;
  let%bind () =
    Program.run
      program
      ~input:(Pipe.of_list [])
      ~output:(Pipe.create_writer (Fn.const (return ())))
  in
  printf "%d\n" program.(0);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 12490719 |}]
;;

let try_ program ~noun ~verb =
  let program = Program.copy program in
  program.(1) <- noun;
  program.(2) <- verb;
  let%map () =
    Program.run
      program
      ~input:(Pipe.of_list [])
      ~output:(Pipe.create_writer (Fn.const (return ())))
  in
  program.(0)
;;

let b () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  let deferreds = ref [] in
  for noun = 0 to 99 do
    for verb = 0 to 99 do
      deferreds
      := (match%map try_ (Program.copy program) ~noun ~verb with
        | 19690720 -> printf "%d\n" ((100 * noun) + verb)
        | _ -> ())
         :: !deferreds
    done
  done;
  Deferred.List.all_unit !deferreds
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 2003 |}]
;;
