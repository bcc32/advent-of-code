open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "aoc.in" >>| String.split_lines >>| List.map ~f:Int.of_string)
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
  [%expect {| 1010299 |}];
  return ()
;;

let find_three nums =
  with_return (fun { return } ->
    List.iteri nums ~f:(fun i1 x ->
      List.iteri nums ~f:(fun i2 y ->
        List.iteri nums ~f:(fun i3 z ->
          if i1 <> i2 && i2 <> i3 && i1 <> i3 && x + y + z = 2020 then return (x, y, z))));
    assert false)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let x, y, z = find_three input in
  print_s [%sexp (x * y * z : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 42140160 |}];
  return ()
;;
