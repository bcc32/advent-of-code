open! Core
open! Async
open! Import
open Intcode

let debug = false
let input () = Reader.file_contents "aoc.in" >>| Program.of_string

let scan x y ~program =
  Program.Sync.provide_input program x;
  Program.Sync.provide_input program y;
  match Program.Sync.step program with
  | Done | Need_input -> failwith "expected output"
  | Output x -> x
;;

let a () =
  let%bind program = input () in
  let snapshot = Program.snapshot program in
  let count = ref 0 in
  for x = 0 to 49 do
    for y = 0 to 49 do
      Program.restore program ~from:snapshot;
      match scan x y ~program with
      | 0 -> ()
      | 1 ->
        incr count;
        ()
      | _ -> assert false
    done
  done;
  printf "%d\n" !count;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 141 |}];
  return ()
;;

let ship_size = 100
let naturals ~from = Sequence.unfold ~init:from ~f:(fun n -> Some (n, n + 1))

let%expect_test "map" =
  let%bind program = input () in
  let snapshot = Program.snapshot program in
  for y = 0 to 50 do
    for x = 0 to 50 do
      Program.restore program ~from:snapshot;
      match scan x y ~program with
      | 0 -> print_char '.'
      | 1 -> print_char '#'
      | _ -> failwith ""
    done;
    print_newline ()
  done;
  [%expect
    {|
    #..................................................
    ...................................................
    ...................................................
    ...................................................
    ...................................................
    ......#............................................
    .......#...........................................
    ........#..........................................
    .........#.........................................
    ..........##.......................................
    ...........##......................................
    ............##.....................................
    .............##....................................
    ...............##..................................
    ................##.................................
    .................##................................
    ..................##...............................
    ...................##..............................
    ....................###............................
    .....................###...........................
    ......................###..........................
    .......................###.........................
    ........................####.......................
    .........................####......................
    ..........................####.....................
    ............................###....................
    .............................####..................
    ..............................####.................
    ...............................####................
    ................................####...............
    .................................#####.............
    ..................................#####............
    ...................................#####...........
    ....................................#####..........
    .....................................#####.........
    ......................................######.......
    .......................................######......
    ........................................######.....
    ..........................................#####....
    ...........................................######..
    ............................................######.
    .............................................######
    ..............................................#####
    ...............................................####
    ................................................###
    .................................................##
    ..................................................#
    ...................................................
    ...................................................
    ...................................................
    ................................................... |}];
  return ()
;;

let b () =
  let%bind program = input () in
  let snapshot = Program.snapshot program in
  let scan =
    Memo.general
      (fun (x, y) ->
        Program.restore program ~from:snapshot;
        scan x y ~program <> 0)
      ~hashable:
        (Hashtbl.Hashable.of_key
           (module struct
             type t = int * int [@@deriving compare, hash, sexp_of]
           end))
  in
  assert (scan (0, 0));
  (* The closest point other than (0, 0) is (6, 5). *)
  assert (scan (6, 5));
  (* The [y_min] values are monotonically non-decreasing for increasing [x]. *)
  let rec y_min =
    let cache = Int.Table.create () in
    fun x ->
      Hashtbl.findi_or_add cache x ~default:(fun x ->
        let from = if x <= 6 then 0 else y_min (x - 1) in
        naturals ~from |> Sequence.find_exn ~f:(fun y -> scan (x, y)))
  in
  let rec y_max =
    let cache = Int.Table.create () in
    fun x ->
      Hashtbl.findi_or_add cache x ~default:(fun x ->
        let from = if x <= 6 then y_min x else y_max (x - 1) in
        naturals ~from |> Sequence.find_exn ~f:(fun y -> not (scan (x, y))))
  in
  (* Try to fit Santa's ship in [x_min, x_min+ship_size).  If it can be done,
     report the minimum y coordinate, such that [scan (x, y)] for all (x, y) in
     [x_min, x_min+ship_size) × [y_min, y_min+ship_size). *)
  let try_fit ~x_min =
    let y_min, y_max =
      Sequence.range x_min (x_min + ship_size)
      |> Sequence.map ~f:(fun x -> y_min x, y_max x)
      |> Sequence.reduce_exn ~f:(fun (y_min, y_max) (y_min', y_max') ->
        (* Tighten the bounds *)
        Int.max y_min y_min', Int.min y_max y_max')
    in
    if y_max - y_min >= ship_size then Some y_min else None
  in
  let x_min, y_min =
    naturals ~from:6
    |> Sequence.find_map ~f:(fun x_min ->
      match try_fit ~x_min with
      | None -> None
      | Some y_min -> Some (x_min, y_min))
    |> Option.value_exn
  in
  if debug then print_s [%message (x_min : int) (y_min : int)];
  printf "%d\n" ((10_000 * x_min) + y_min);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 15641348 |}];
  return ()
;;
