open! Core
open! Async
open! Import

let limits () =
  let%map x, y =
    Reader.file_contents "input" >>| String.strip >>| String.lsplit2_exn ~on:'-'
  in
  Int.of_string x, Int.of_string y
;;

let digit_groups s =
  s
  |> String.to_list
  |> Sequence.of_list
  |> Sequence.group ~break:Char.( <> )
  |> Sequence.map ~f:(fun xs -> List.hd_exn xs, List.length xs)
;;

let is_nondecreasing s = s |> String.to_list |> List.is_sorted ~compare:[%compare: char]

let a () =
  let%bind x, y = limits () in
  let c = ref 0 in
  for n = x to y do
    let n = Int.to_string n in
    if is_nondecreasing n && digit_groups n |> Sequence.exists ~f:(fun (_, c) -> c > 1)
    then incr c
  done;
  printf "%d\n" !c;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 579 |}]
;;

let b () =
  let%bind x, y = limits () in
  let c = ref 0 in
  for n = x to y do
    let n = Int.to_string n in
    if is_nondecreasing n && digit_groups n |> Sequence.exists ~f:(fun (_, c) -> c = 2)
    then incr c
  done;
  printf "%d\n" !c;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 358 |}]
;;
