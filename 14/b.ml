open! Core
open! Async
open! Import

let main () =
  let%bind input = Reader.file_contents "input" >>| Int.of_string in
  let length = String.length (Int.to_string input) in
  let ds = Cocoa.make_sequence () in
  let prefix, ds = Sequence.split_n ds length in
  let i, _ =
    Sequence.folding_map ds ~init:(prefix |> Fqueue.of_list) ~f:(fun digits d ->
      ( digits |> Fqueue.discard_exn |> Fn.flip Fqueue.enqueue d
      , digits |> Fqueue.to_list |> List.fold ~init:0 ~f:(fun acc d -> (10 * acc) + d)
      ))
    |> Sequence.findi ~f:(fun _ n -> n = input)
    |> Option.value_exn
  in
  printf "%d\n" i;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 20203532 |}]
;;
