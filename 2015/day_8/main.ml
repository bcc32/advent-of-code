open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| String.split_lines)
;;

let eval_string_length str =
  let str = String.sub str ~pos:1 ~len:(String.length str - 2) in
  let rec start ~pos ~count =
    if pos >= String.length str
    then count
    else if Char.( = ) str.[pos] '\\'
    then escape ~pos:(pos + 1) ~count
    else start ~pos:(pos + 1) ~count:(count + 1)
  and escape ~pos ~count =
    if Char.( = ) str.[pos] '\\' || Char.( = ) str.[pos] '"'
    then start ~pos:(pos + 1) ~count:(count + 1)
    else start ~pos:(pos + 3) ~count:(count + 1)
  in
  start ~pos:0 ~count:0
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let x =
    List.sum (module Int) input ~f:(fun s -> String.length s - eval_string_length s)
  in
  print_s [%sexp (x : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 1371 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let x =
    List.sum
      (module Int)
      input
      ~f:(fun s ->
        2 + String.count s ~f:(fun c -> List.mem [ '"'; '\\' ] c ~equal:Char.equal))
  in
  print_s [%sexp (x : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 2117 |}];
  return ()
;;
