open! Core
open! Async
open! Import

let main () =
  let%bind steps =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Dep.of_string |> Pipe.to_list)
  in
  let steps = Dep.topo_sort steps in
  printf "%s\n" (String.concat steps);
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| BCEFLDMQTXHZGKIASVJYORPUWN |}]
;;
