open! Core
open! Async
open! Import

let limits () =
  let%map x, y =
    Reader.file_contents "input" >>| String.strip >>| String.lsplit2_exn ~on:'-'
  in
  Int.of_string x, Int.of_string y
;;

let has_consec_digits s =
  s
  |> String.to_list
  |> List.find_consecutive_duplicate ~equal:Char.equal
  |> Option.is_some
;;

let a () =
  let%bind x, y = limits () in
  let c = ref 0 in
  for n = x to y do
    if n |> Int.to_string |> has_consec_digits
    && n |> Int.to_string |> String.to_list |> List.is_sorted ~compare:[%compare: char]
    then incr c
  done;
  printf "%d\n" !c;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 579 |}]
;;

let has_consec_digits s =
  let r =
    let open Re in
    List.init 10 ~f:(fun i ->
      let d = Char.of_int_exn (Char.to_int '0' + i) in
      seq
        [ alt [ bos; compl [ char d ] ]; char d; char d; alt [ eos; compl [ char d ] ] ])
    |> alt
    |> compile
  in
  Re.execp r s
;;

let b () =
  let%bind x, y = limits () in
  let c = ref 0 in
  for n = x to y do
    if n |> Int.to_string |> has_consec_digits
    && n |> Int.to_string |> String.to_list |> List.is_sorted ~compare:[%compare: char]
    then incr c
  done;
  printf "%d\n" !c;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 358 |}]
;;
