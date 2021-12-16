open! Core
open! Async
open! Import

type dir =
  | L
  | R
  | U
  | D

let dir_of_char = function
  | '<' -> L
  | '>' -> R
  | '^' -> U
  | 'v' -> D
  | _ -> failwith "invalid dir"
;;

let move (x, y) dir =
  match dir with
  | U -> x, y + 1
  | D -> x, y - 1
  | L -> x - 1, y
  | R -> x + 1, y
;;

let input () =
  Reader.file_contents "input.txt" >>| String.to_list >>| List.map ~f:dir_of_char
;;

module Pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

let a () =
  let%bind dirs = input () in
  let coord = ref (0, 0) in
  let visited = Pair.Hash_set.create () in
  Hash_set.add visited !coord;
  List.iter dirs ~f:(fun dir ->
    coord := move !coord dir;
    Hash_set.add visited !coord);
  print_s [%sexp (Hash_set.length visited : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 2592 |}] in
  return ()
;;

let b () =
  let%bind dirs = input () in
  let coord_santa = ref (0, 0) in
  let coord_robo_santa = ref (0, 0) in
  let visited = Pair.Hash_set.create () in
  Hash_set.add visited !coord_santa;
  Hash_set.add visited !coord_robo_santa;
  List.iteri dirs ~f:(fun i dir ->
    if i % 2 = 0
    then (
      coord_santa := move !coord_santa dir;
      Hash_set.add visited !coord_santa)
    else (
      coord_robo_santa := move !coord_robo_santa dir;
      Hash_set.add visited !coord_robo_santa));
  print_s [%sexp (Hash_set.length visited : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 2360 |}] in
  return ()
;;
