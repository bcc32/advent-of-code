open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "aoc.in" >>| Yojson.Basic.from_string)
;;

let rec sum_nums json =
  match (json : Yojson.Basic.t) with
  | `Bool _ -> 0.
  | `Null -> 0.
  | `Assoc kvs -> List.sum (module Float) kvs ~f:(fun (_, x) -> sum_nums x)
  | `List xs -> List.sum (module Float) xs ~f:sum_nums
  | `Float f -> f
  | `String _ -> 0.
  | `Int i -> float i
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let x = sum_nums input in
  print_s [%sexp (x : float)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 111754 |}];
  return ()
;;

let rec sum_nums_except_red json =
  match (json : Yojson.Basic.t) with
  | `Bool _ -> 0.
  | `Null -> 0.
  | `Assoc kvs ->
    if List.exists kvs ~f:(fun (_, v) -> Yojson.Basic.equal v (`String "red"))
    then 0.
    else List.sum (module Float) kvs ~f:(fun (_, x) -> sum_nums_except_red x)
  | `List xs -> List.sum (module Float) xs ~f:sum_nums_except_red
  | `Float f -> f
  | `String _ -> 0.
  | `Int i -> float i
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let x = sum_nums_except_red input in
  print_s [%sexp (x : float)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 65402 |}];
  return ()
;;
