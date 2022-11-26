open! Core
open! Async
open! Import

let main () =
  let%bind points =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Point.of_string |> Pipe.to_list)
  in
  let points =
    Points.to_sequence points
    |> Sequence.fold_until
         ~init:([], Int.max_value)
         ~f:(fun (last_points, last_area) (points, area) ->
           if area > last_area then Stop last_points else Continue (points, area))
         ~finish:(fun _ -> assert false)
  in
  printf !"%{Points#plot}" points;
  return ()
;;

(* RRANZLAC *)
let%expect_test "a" =
  let%bind () = main () in
  [%expect
    {|
    #####...#####.....##....#....#..######..#.........##.....####.
    #....#..#....#...#..#...##...#.......#..#........#..#...#....#
    #....#..#....#..#....#..##...#.......#..#.......#....#..#.....
    #....#..#....#..#....#..#.#..#......#...#.......#....#..#.....
    #####...#####...#....#..#.#..#.....#....#.......#....#..#.....
    #..#....#..#....######..#..#.#....#.....#.......######..#.....
    #...#...#...#...#....#..#..#.#...#......#.......#....#..#.....
    #...#...#...#...#....#..#...##..#.......#.......#....#..#.....
    #....#..#....#..#....#..#...##..#.......#.......#....#..#....#
    #....#..#....#..#....#..#....#..######..######..#....#...####. |}];
  return ()
;;
