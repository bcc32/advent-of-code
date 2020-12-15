open! Core
open! Async
open! Import

module Input = struct
  open! Advent_of_code_input_helpers

  type t = int list [@@deriving sexp_of]

  let parse input : t = input |> String.strip |> words ~sep:"," |> List.map ~f:stoi

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let play numbers =
  Sequence.unfold_with_and_finish
    (Sequence.of_list numbers)
    ~init:(0, Map.empty (module Int), None)
    ~running_step:(fun (i, most_recent_index, _last_number_most_recent_index) number ->
      Yield
        ( number
        , ( i + 1
          , Map.set most_recent_index ~key:number ~data:i
          , Map.find most_recent_index number ) ))
    ~inner_finished:Fn.id
    ~finishing_step:(fun (i, most_recent_index, last_number_most_recent_index) ->
      match last_number_most_recent_index with
      | None ->
        Yield
          ( 0
          , (i + 1, Map.set most_recent_index ~key:0 ~data:i, Map.find most_recent_index 0)
          )
      | Some i' ->
        let diff = i - i' - 1 in
        Yield
          ( diff
          , ( i + 1
            , Map.set most_recent_index ~key:diff ~data:i
            , Map.find most_recent_index diff ) ))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let ans = play input |> Fn.flip Sequence.nth_exn 2019 in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 1238 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let ans = play input |> Fn.flip Sequence.nth_exn 29999999 in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 3745954 |}] in
  return ()
;;
