open! Core
open! Async
open! Import

let fuel ~mass = (mass / 3) - 2

let main () =
  let%bind lines = Reader.file_lines "input" in
  lines
  |> List.map ~f:Int.of_string
  |> List.sum (module Int) ~f:(fun mass -> fuel ~mass)
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 3394032 |}]
;;
