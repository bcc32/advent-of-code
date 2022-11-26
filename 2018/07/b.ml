open! Core
open! Async
open! Import

let main () =
  let%bind steps =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Dep.of_string |> Pipe.to_list)
  in
  let time =
    Dep.schedule steps ~workers:5 ~cost:(fun step ->
      Char.to_int step.[0] - Char.to_int 'A' + 61)
  in
  printf "%d\n" time;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 987 |}];
  return ()
;;
