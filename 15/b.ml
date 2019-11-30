open! Core
open! Async
open! Import

let main () =
  let%bind _lines =
    Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list)
  in
  printf "output\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| output |}]
;;
