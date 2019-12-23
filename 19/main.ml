open! Core
open! Async
open! Import
open Intcode

let debug = false
let input () = Reader.file_contents "input" >>| Program.of_string

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
  [%expect {| 141 |}]
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
    ................................................... |}]
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
  let y_min =
    Memo.general (fun x ->
      naturals ~from:0 |> Sequence.find_exn ~f:(fun y -> scan (x, y)))
  in
  let y_max =
    Memo.general (fun x ->
      naturals ~from:(y_min x) |> Sequence.find_exn ~f:(fun y -> not (scan (x, y))))
  in
  (* Try to fit Santa's ship in [x_min, x_min+ship_size).  If it can be done, report the
     minimum y coordinate, such that [scan (x, y)] for all (x, y) in [x_min,
     x_min+ship_size) × [y_min, y_min+ship_size). *)
  let try_fit ~x_min =
    let y_min, y_max =
      Sequence.range x_min (x_min + ship_size)
      |> Sequence.map ~f:(fun x -> y_min x, y_max x)
      |> Sequence.reduce ~f:(fun (y_min, y_max) (y_min', y_max') ->
        (* Tighten the bounds *)
        Int.max y_min y_min', Int.min y_max y_max')
      |> Option.value_exn
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
  [%expect {| 15641348 |}]
;;
