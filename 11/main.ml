open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

type dir =
  | N
  | S
  | E
  | W

let apply_turn dir turn =
  if turn = 0
  then (
    match dir with
    | N -> W
    | S -> E
    | E -> N
    | W -> S)
  else (
    match dir with
    | N -> E
    | S -> W
    | E -> S
    | W -> N)
;;

let move_forward (x, y) dir =
  match dir with
  | N -> x, y + 1
  | S -> x, y - 1
  | E -> x + 1, y
  | W -> x - 1, y
;;

let paint program ~starting =
  let loc = ref (0, 0) in
  let dir = ref N in
  let paint =
    Hashtbl.create
      (module struct
        type t = int * int [@@deriving compare, hash, sexp_of]
      end)
  in
  Hashtbl.set paint ~key:!loc ~data:starting;
  let robot_input_r, robot_input_w = Pipe.create () in
  let robot_output_r, robot_output_w = Pipe.create () in
  let robot_done = Program.run program ~input:robot_input_r ~output:robot_output_w in
  Pipe.write_without_pushback robot_input_w starting;
  let%bind () =
    Deferred.repeat_until_finished () (fun () ->
      match%bind Pipe.read_exactly robot_output_r ~num_values:2 with
      | `Fewer _ -> failwith "eof"
      | `Exactly elts ->
        let color = Queue.get elts 0 in
        let turn = Queue.get elts 1 in
        Hashtbl.set paint ~key:!loc ~data:color;
        dir := apply_turn !dir turn;
        loc := move_forward !loc !dir;
        Pipe.write_without_pushback
          robot_input_w
          (Hashtbl.find paint !loc |> Option.value ~default:0);
        return (`Repeat ())
      | `Eof -> return (`Finished ()))
  in
  let%bind () = robot_done in
  return (Hashtbl.length paint, paint)
;;

let a () =
  let%bind program = input () in
  let%bind n, _ = paint program ~starting:0 in
  printf "%d\n" n;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 1747 |}]
;;

let b () =
  let%bind program = input () in
  let%bind _, grid = paint program ~starting:1 in
  let minx =
    grid |> Hashtbl.keys |> List.map ~f:fst |> List.min_elt ~compare:[%compare: int] |> uw
  in
  let maxx =
    grid |> Hashtbl.keys |> List.map ~f:fst |> List.max_elt ~compare:[%compare: int] |> uw
  in
  let miny =
    grid |> Hashtbl.keys |> List.map ~f:snd |> List.min_elt ~compare:[%compare: int] |> uw
  in
  let maxy =
    grid |> Hashtbl.keys |> List.map ~f:snd |> List.max_elt ~compare:[%compare: int] |> uw
  in
  for y = maxy downto miny do
    for x = minx to maxx do
      match Hashtbl.find grid (x, y) with
      | None | Some 0 -> print_char '.'
      | Some 1 -> print_char '#'
      | _ -> assert false
    done;
    print_newline ()
  done;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect
    {|
    .####..##...##..###..#..#.#..#.#....###....
    ....#.#..#.#..#.#..#.#..#.#.#..#....#..#...
    ...#..#....#....#..#.####.##...#....###....
    ..#...#....#.##.###..#..#.#.#..#....#..#...
    .#....#..#.#..#.#.#..#..#.#.#..#....#..#...
    .####..##...###.#..#.#..#.#..#.####.###.... |}]
;;
