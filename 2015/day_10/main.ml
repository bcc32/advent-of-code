open! Core
open! Async
open! Import

let rle seq ~equal =
  let open Sequence.Generator.Let_syntax in
  let rec start seq =
    match Sequence.next seq with
    | None -> return ()
    | Some (hd, tl) -> keep tl hd 1
  and keep seq x count =
    match Sequence.next seq with
    | Some (hd, tl) when equal x hd -> keep tl x (count + 1)
    | None | Some _ ->
      let%bind () = Sequence.Generator.yield (x, count) in
      start seq
  in
  Sequence.Generator.run (start seq)
;;

let look_and_say s =
  let buf = Buffer.create 16 in
  Sequence.iter
    (rle (Sequence.of_seq (Caml.String.to_seq s)) ~equal:Char.equal)
    ~f:(fun (char, count) -> bprintf buf "%d%c" count char);
  Buffer.contents buf
;;

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| String.rstrip)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = Fn.apply_n_times look_and_say ~n:40 input in
  print_s [%sexp (String.length ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 492982 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = Fn.apply_n_times look_and_say ~n:50 input in
  print_s [%sexp (String.length ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 6989950 |}];
  return ()
;;
