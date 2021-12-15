open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| (String.rstrip >> Int.of_string))
;;

let whole_numbers_with_divisor_sum =
  let whole = Sequence.unfold_step ~init:1 ~f:(fun n -> Yield (n, n + 1)) in
  whole
  |> Sequence.map ~f:(fun n ->
    ( n
    , whole
      |> Sequence.take_while ~f:(fun d -> d * d <= n)
      |> Sequence.filter ~f:(fun d -> n % d = 0)
      |> Sequence.sum (module Int) ~f:(fun d -> d + (n / d)) ))
;;

let whole_numbers_with_divisor_sum_upto_50 =
  let whole = Sequence.unfold_step ~init:1 ~f:(fun n -> Yield (n, n + 1)) in
  whole
  |> Sequence.map ~f:(fun n ->
    ( n
    , whole
      |> Sequence.take_while ~f:(fun d -> d * d <= n)
      |> Sequence.filter ~f:(fun d -> n % d = 0)
      |> Sequence.sum
           (module Int)
           ~f:(fun d ->
             let d' = n / d in
             (if d > 50 then 0 else d') + if d' > 50 then 0 else d) ))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans =
    Sequence.find_map whole_numbers_with_divisor_sum ~f:(fun (n, divsum) ->
      Option.some_if (10 * divsum >= input) n)
    |> Option.value_exn
  in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 831600 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans =
    Sequence.find_map whole_numbers_with_divisor_sum_upto_50 ~f:(fun (n, divsum) ->
      Option.some_if (11 * divsum >= input) n)
    |> Option.value_exn
  in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 884520 |}] in
  return ()
;;
