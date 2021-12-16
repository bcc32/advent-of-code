open! Core
open! Async
open! Import

let input = Lazy_deferred.create (fun () -> Reader.file_contents "input.txt")

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  print_s [%sexp (String.length input : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 8 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  print_s [%sexp (String.length input : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 8 |}] in
  return ()
;;
