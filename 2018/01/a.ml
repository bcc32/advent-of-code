open! Core
open! Async
open! Import

let main () =
  Reader.with_file "input" ~f:(fun r ->
    Reader.lines r
    |> Pipe.fold_without_pushback ~init:0 ~f:(fun acc line -> acc + Int.of_string line))
  >>| printf "%d\n"
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 590 |}]
;;
