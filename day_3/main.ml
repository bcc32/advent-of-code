open! Core
open! Async
open! Import

let parse_input str =
  str
  |> String.split_lines
  |> List.map ~f:(fun line -> line |> String.to_array)
  |> Array.of_list
;;

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse_input)
;;

let a () =
  let%bind grid = Lazy_deferred.force_exn input in
  let pos = ref (0, 0) in
  let trees = ref 0 in
  while fst !pos < Array.length grid do
    if Char.equal grid.(fst !pos).(snd !pos % Array.length grid.(0)) '#' then incr trees;
    pos := fst !pos + 1, snd !pos + 3
  done;
  print_s [%sexp (trees : int ref)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 151 |}] in
  return ()
;;

let try_slope grid (r, d) =
  let pos = ref (0, 0) in
  let trees = ref 0 in
  while fst !pos < Array.length grid do
    if Char.equal grid.(fst !pos).(snd !pos % Array.length grid.(0)) '#' then incr trees;
    pos := fst !pos + d, snd !pos + r
  done;
  !trees
;;

let b () =
  let%bind grid = Lazy_deferred.force_exn input in
  List.map [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ] ~f:(fun (r, d) -> try_slope grid (r, d))
  |> List.reduce_exn ~f:( * )
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 7540141059 |}] in
  return ()
;;
