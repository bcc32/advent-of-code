open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| String.strip)
;;

let hash ~secret_key ~number =
  Md5.digest_string (secret_key ^ Int.to_string number) |> Md5.to_hex
;;

let mine secret_key =
  let rec loop number =
    if String.is_prefix (hash ~secret_key ~number) ~prefix:"00000"
    then number
    else loop (number + 1)
  in
  loop 1
;;

let%expect_test "abcdef609043" =
  print_s [%sexp (hash ~secret_key:"abcdef" ~number:609043 : string)];
  let%bind () = [%expect {| 000001dbbfa3a5c83a2d506429c7b00e |}] in
  return ()
;;

let%expect_test "mine(abcdef)" =
  print_s [%sexp (mine "abcdef" : int)];
  let%bind () = [%expect {| 609043 |}] in
  return ()
;;

let%expect_test "mine(pqrstuv)" =
  print_s [%sexp (mine "pqrstuv" : int)];
  let%bind () = [%expect {| 1048970 |}] in
  return ()
;;

let a () =
  let%bind secret_key = Lazy_deferred.force_exn input in
  let ans = mine secret_key in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 117946 |}] in
  return ()
;;

let mine secret_key =
  let rec loop number =
    if String.is_prefix (hash ~secret_key ~number) ~prefix:"000000"
    then number
    else loop (number + 1)
  in
  loop 1
;;

let b () =
  let%bind secret_key = Lazy_deferred.force_exn input in
  let ans = mine secret_key in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 3938038 |}] in
  return ()
;;
