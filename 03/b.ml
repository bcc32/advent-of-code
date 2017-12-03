open! Core

let iter_neighbors (x, y) ~f =
  f (x + 1) y;
  f (x + 1) (y + 1);
  f x (y + 1);
  f (x - 1) (y + 1);
  f (x - 1) y;
  f (x - 1) (y - 1);
  f x (y - 1);
  f (x + 1) (y - 1);
;;

let iter_coordinates ~f =
  let rec loop_ring x y =
    let ring = x in
    for y = y to y + 2 * ring - 1 do f x y done;
    let y = y + 2 * ring - 1 in
    for x = x - 1 downto x - 2 * ring do f x y done;
    let x = x - 2 * ring in
    for y = y - 1 downto y - 2 * ring do f x y done;
    let y = y - 2 * ring in
    for x = x + 1 to x + 2 * ring do f x y done;
    let x = x + 2 * ring in
    loop_ring (x + 1) y
  in
  loop_ring 1 0
;;

module Coord = Tuple.Hashable(Int)(Int)

let memory = Coord.Table.create ()

let () = Hashtbl.add_exn memory ~key:(0, 0) ~data:1

let () =
  let threshold =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_all file
      |> String.strip
      |> Int.of_string)
  in
  iter_coordinates ~f:(fun x y ->
    let sum = ref 0 in
    iter_neighbors (x, y) ~f:(fun x2 y2 ->
      Hashtbl.find memory (x2, y2)
      |> Option.iter ~f:(fun v -> sum := !sum + v));
    if !sum > threshold
    then (
      printf "%d\n" !sum;
      exit 0);
    Hashtbl.add_exn memory ~key:(x, y) ~data:!sum)
;;
