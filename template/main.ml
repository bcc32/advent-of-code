open! Core
open! Async
open! Import

module Input = struct
  open! Advent_of_code_input_helpers

  type t = string [@@deriving sexp_of]

  let parse input : t = input

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  print_s [%sexp (input : Input.t)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| "example\n" |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  print_s [%sexp (input : Input.t)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| "example\n" |}] in
  return ()
;;
