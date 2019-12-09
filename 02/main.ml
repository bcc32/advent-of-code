open! Core
open! Async
open! Import
open Intcode

let a () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
  program.memory.(1) <- 12;
  program.memory.(2) <- 2;
  let%bind () = Program.run_without_io program in
  printf "%d\n" program.memory.(0);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 12490719 |}]
;;

let try_ program ~noun ~verb =
  let program = Program.copy program in
  program.memory.(1) <- noun;
  program.memory.(2) <- verb;
  let%map () = Program.run_without_io program in
  program.memory.(0)
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
