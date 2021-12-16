open! Core
open! Async
open! Import

let a () =
  let%bind input = Reader.file_contents "input.txt" in
  let count = ref 0 in
  String.iter input ~f:(function
    | '(' -> incr count
    | ')' -> decr count
    | _ -> assert false);
  print_s [%sexp (!count : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 232 |}] in
  return ()
;;

let b () =
  let%bind input = Reader.file_contents "input.txt" in
  let count = ref 0 in
  with_return (fun { return } ->
    for i = 0 to String.length input - 1 do
      match input.[i] with
      | '(' -> incr count
      | ')' ->
        decr count;
        if !count < 0 then return (i + 1)
      | _ -> assert false
    done;
    assert false)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 1783 |}] in
  return ()
;;
