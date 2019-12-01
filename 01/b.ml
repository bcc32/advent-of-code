open! Core
open! Async
open! Import

let rec fuel ~mass =
  let f = (mass / 3) - 2 in
  if f <= 0 then 0 else f + fuel ~mass:f
;;

let main () =
  let%bind lines = Reader.file_lines "input" in
  lines
  |> List.map ~f:Int.of_string
  |> List.sum (module Int) ~f:(fun mass -> fuel ~mass)
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 5088176 |}]
;;
