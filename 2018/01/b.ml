open! Core
open! Async
open! Import

let main () =
  let%bind deltas =
    Reader.with_file "input" ~f:(fun r ->
      Reader.lines r |> Pipe.map ~f:Int.of_string |> Pipe.to_list)
  in
  Sequence.cycle_list_exn deltas
  |> Sequence.folding_map ~init:0 ~f:(fun sum x -> sum + x, sum)
  |> Sequence.fold_until
       ~init:Int.Set.empty
       ~f:(fun visited x ->
         if Set.mem visited x then Stop x else Continue (Set.add visited x))
       ~finish:(fun s -> raise_s [%message "no dup" (s : Int.Set.t)])
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 83445 |}]
;;
