open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let scan x y ~program =
  Program.Sync.provide_input program x;
  Program.Sync.provide_input program y;
  match Program.Sync.step program with
  | Done | Need_input -> failwith "expected output"
  | Output x -> x
;;

let a () =
  let%bind program = input () in
  let count = ref 0 in
  for x = 0 to 49 do
    for y = 0 to 49 do
      match scan x y ~program:(Program.copy program) with
      | 0 -> ()
      | 1 ->
        incr count;
        ()
      | _ -> assert false
    done
  done;
  printf "%d\n" !count;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 141 |}]
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
