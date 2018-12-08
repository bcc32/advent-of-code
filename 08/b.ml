open! Core
open! Async
open! Import

let main () =
  let%bind nums =
    Reader.file_contents "input"
    >>| String.strip
    >>| String.split ~on:' '
    >>| List.map ~f:Int.of_string
  in
  Tree.parse nums |> Tree.value |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 21502 |}]
;;
