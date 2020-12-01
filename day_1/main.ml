open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| String.split_lines
    >>| List.map ~f:Int.of_string)
;;

let find_two nums =
  let n = Hash_set.create (module Int) in
  with_return (fun { return } ->
    List.iter nums ~f:(fun x ->
      if Hash_set.mem n (2020 - x) then return (x, 2020 - x);
      Hash_set.add n x);
    assert false)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let x, y = find_two input in
  print_s [%sexp (x * y : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 1010299 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  print_s [%sexp (List.length input : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 200 |}] in
  return ()
;;
