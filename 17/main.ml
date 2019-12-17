open! Core
open! Async
open! Import
open Intcode

let debug = false
let input () = Reader.file_contents "input" >>| Program.of_string

let output program =
  match Program.run program with
  | { input = _; output; done_ = _ } ->
    let buffer = Buffer.create 0 in
    let%bind () =
      Pipe.iter_without_pushback output ~f:(fun c ->
        Buffer.add_char buffer (Char.of_int_exn c))
    in
    (* TODO: Investigate *)
    return (Buffer.contents buffer |> String.strip |> String.split_lines |> Array.of_list)
;;

let a () =
  let%bind program = input () in
  let%bind output = output program in
  let sum = ref 0 in
  for i = 1 to Array.length output - 2 do
    for j = 1 to String.length output.(i) - 2 do
      let is_scaffold (x, y) = Char.( = ) output.(i + x).[j + y] '#' in
      if List.for_all [ -1, 0; 1, 0; 0, -1; 0, 1; 0, 0 ] ~f:is_scaffold
      then sum := !sum + (i * j)
    done
  done;
  printf "%d\n" !sum;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2660 |}]
;;

let show_map () =
  let%bind program = input () in
  let%bind map = output (Program.copy program) in
  Array.iter map ~f:print_endline;
  return ()
;;

let%expect_test "map" =
  let%bind () = show_map () in
  [%expect
    {|
    ..............###########.###########................
    ..............#.........#.#.........#................
    ..............#.......#########.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.#########.#...#.....#................
    ..............#.#.....#...#...#.....#................
    ..............#.#.....#...#########.#................
    ..............#.#.....#.......#...#.#................
    ..............#########.......#######................
    ................#.................#..................
    ................#...........#########................
    ................#...........#.....#.#................
    ................#########...#.....#.#................
    ........................#...#.....#.#................
    ........................#...#.....#.#................
    ........................#...#.......#................
    ........................#...#.......#######..........
    ........................#...#.............#..........
    ....................#########.............#..........
    ....................#...#.................#..........
    ..................#######.................#..........
    ..................#.#.....................#..........
    ..................#.#.....................#..........
    ..................#.#.....................#..........
    ..................#.#.....................##########^
    ..................#.#................................
    #######...#########.#................................
    #.....#...#.........#................................
    #.....#...#.#########................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#######........................................
    #.........#..........................................
    ###########.......................................... |}]
;;

(*
   A = L,8,L,8,R,8
   B = L,10,R,8,L,6,R,6
   C = R,8,L,6,L,10,L,10

   L,10,R,8,L,6,R,6,L,8,L,8,R,8,L,10,
   R,8,L,6,R,6,R,8,L,6,L,10,L,10,L,10,
   R,8,L,6,R,6,L,8,L,8,R,8,R,8,L,6,L,10,
   L,10,L,8,L,8,R,8,R,8,L,6,L,10,L,10,L,8,L,8,R,8

   B,A,B,C,B,A,C,A,C,A *)

let b () =
  let%bind program = input () in
  program.memory.(0) <- 2;
  match Program.run program with
  | { input; output; done_ = _ } ->
    let write_string_line str =
      String.iter (str ^ "\n") ~f:(fun c ->
        Pipe.write_without_pushback input (Char.to_int c))
    in
    write_string_line "B,A,B,C,B,A,C,A,C,A";
    write_string_line "L,8,L,8,R,8";
    write_string_line "L,10,R,8,L,6,R,6";
    write_string_line "R,8,L,6,L,10,L,10";
    if debug
    then (
      write_string_line "y";
      Pipe.iter_without_pushback output ~f:(fun c -> print_char (Char.of_int_exn c)))
    else (
      write_string_line "n";
      Pipe.iter_without_pushback output ~f:(fun c ->
        if c > Char.to_int Char.max_value then printf "%d\n" c))
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 790595 |}]
;;
