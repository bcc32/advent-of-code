open! Core
open! Async
open! Import

let main () =
  let%bind nums =
    Reader.file_contents "aoc.in"
    >>| String.strip
    >>| String.split ~on:' '
    >>| List.map ~f:Int.of_string
  in
  Tree.parse nums |> Tree.sum_metadata |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 43351 |}];
  return ()
;;
