open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| List.map ~f:Int.of_string)
;;

let rec iter_combinations list ~f =
  match list with
  | [] -> f []
  | hd :: tl ->
    iter_combinations tl ~f:(fun combo ->
      f combo;
      f (hd :: combo))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let count = ref 0 in
  iter_combinations input ~f:(fun i ->
    if List.sum (module Int) i ~f:Fn.id = 150 then incr count);
  print_s [%sexp (!count : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 654 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let count = ref 0 in
  let min_size = ref Int.max_value in
  iter_combinations input ~f:(fun i ->
    if List.sum (module Int) i ~f:Fn.id = 150
    then
      if List.length i = !min_size
      then incr count
      else if List.length i < !min_size
      then (
        count := 1;
        min_size := List.length i));
  print_s [%sexp (!count : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 57 |}];
  return ()
;;
