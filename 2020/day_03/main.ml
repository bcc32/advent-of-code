open! Core
open! Async
open! Import
module Coord = Advent_of_code_lattice_geometry.Coord
module Grid = Advent_of_code_lattice_geometry.Grid

module Input = struct
  open! Advent_of_code_input_helpers

  type t = char Grid.t

  let parse = grid ~f:Fn.id

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let count_trees grid ~slope:(r, d) =
  let open Grid.O in
  let coord = ref (Coord.RC.create ~row:0 ~col:0) in
  let trees = ref 0 in
  while grid.?(!coord) do
    if Char.equal '#' grid.%(!coord) then incr trees;
    coord
      := Coord.RC.update !coord ~f:(fun { x = row; y = col } ->
           { x = row + d; y = (col + r) % Grid.width grid })
  done;
  !trees
;;

let a () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  let trees = count_trees grid ~slope:(3, 1) in
  print_s [%sexp (trees : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 151 |}];
  return ()
;;

let b () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  List.map [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ] ~f:(fun slope -> count_trees grid ~slope)
  |> List.reduce_exn ~f:( * )
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 7540141059 |}];
  return ()
;;
