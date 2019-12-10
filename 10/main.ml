open! Core
open! Async
open! Import

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> Array.of_list_map ~f:String.to_array
;;

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let vec (x, y) =
  let g = gcd (Int.abs x) (Int.abs y) in
  x / g, y / g
;;

let scan grid (x, y) =
  let scanned_vecs =
    Hash_set.create
      (module struct
        type t = int * int [@@deriving compare, hash, sexp_of]
      end)
      ()
  in
  for x' = 0 to Array.length grid.(0) - 1 do
    for y' = 0 to Array.length grid - 1 do
      if Char.( = ) grid.(y').(x') '#'
      then (
        try
          let vec = vec (x' - x, y' - y) in
          Hash_set.add scanned_vecs vec
        with
        | _ -> ())
    done
  done;
  Hash_set.length scanned_vecs
;;

let a_coord grid =
  Sequence.range 0 (Array.length grid.(0))
  |> Sequence.cartesian_product (Sequence.range 0 (Array.length grid))
  |> Sequence.filter ~f:(fun (x, y) -> grid.(y).(x) = '#')
  |> Sequence.max_elt ~compare:(Comparable.lift [%compare: int] ~f:(fun v -> scan grid v))
  |> uw
;;

let a () =
  let%bind grid = input () in
  let v = a_coord grid in
  print_s [%sexp (v : int * int)];
  scan grid v |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {|
    (11 13)
    227 |}]
;;

let all_asteroids grid =
  Sequence.range 0 (Array.length grid.(0))
  |> Sequence.cartesian_product (Sequence.range 0 (Array.length grid))
  |> Sequence.filter ~f:(fun (x, y) -> grid.(y).(x) = '#')
  |> Sequence.to_list
;;

let vec2 (x, y) (x', y') = try vec (x-x',  y-y') with |_->0,0

let rot (x, y) (x', y') =
  let dx = float (x' - x) in
  let dy = float (y - y') in
  let theta = Float.atan2 dy dx in
  let theta_from_north = Float.pi /. 2. -. theta in
  if theta_from_north < 0. then theta_from_north +. Float.pi *. 2. else theta_from_north
;;

let dist (x, y) (x', y') =
  let dx = float (x' - x) in
  let dy = float (y - y') in
  Float.hypot dx dy
;;

let b () =
  let%bind grid = input () in
  let v = a_coord grid in
  let ast =
    all_asteroids grid
    |> List.filter ~f:(Fn.non ([%equal: int * int ] v))
    |> List.map ~f:(fun p -> vec2 v p, p)
    |> Map.Poly.of_alist_multi
    |> Map.map ~f:(List.sort ~compare:(Comparable.lift [%compare: float] ~f:(dist v)))
    |> Map.to_alist
    |> List.map ~f:snd
    |> List.sort ~compare:(Comparable.lift [%compare: float] ~f:(fun p -> rot v (List.hd_exn p)))
    |> ref
  in
  let c = ref 0 in
  let x, y =
    with_return (fun { return } ->
      ast
      := List.map !ast ~f:(function
        | [] -> []
        | (x, y) :: tl ->
          print_s [%sexp (x, y : int * int)];
          if !c = 199 then return (x, y);
          incr c;
          tl);
      assert false)
  in
  printf "%d\n" ((100 * x) + y);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {|
    (11 11)
    (12 0)
    (12 1)
    (12 2)
    (12 3)
    (12 5)
    (12 6)
    (13 0)
    (13 1)
    (12 8)
    (13 4)
    (14 0)
    (13 5)
    (14 2)
    (13 6)
    (14 3)
    (15 0)
    (13 7)
    (15 2)
    (14 5)
    (13 8)
    (16 1)
    (15 4)
    (16 2)
    (17 0)
    (12 11)
    (18 0)
    (17 2)
    (15 6)
    (14 8)
    (19 0)
    (16 5)
    (18 2)
    (13 10)
    (20 0)
    (18 3)
    (16 6)
    (14 9)
    (18 4)
    (19 3)
    (20 2)
    (17 6)
    (12 12)
    (20 5)
    (19 6)
    (17 8)
    (16 9)
    (15 10)
    (18 8)
    (14 11)
    (19 8)
    (16 10)
    (18 9)
    (20 8)
    (13 12)
    (20 9)
    (18 10)
    (16 11)
    (19 10)
    (17 11)
    (18 11)
    (15 12)
    (16 12)
    (17 12)
    (19 12)
    (20 12)
    (12 13)
    (20 14)
    (19 14)
    (18 14)
    (16 14)
    (19 15)
    (14 14)
    (19 16)
    (16 15)
    (18 16)
    (13 14)
    (20 18)
    (18 17)
    (16 16)
    (19 18)
    (14 15)
    (15 16)
    (18 19)
    (19 20)
    (12 14)
    (16 19)
    (15 18)
    (16 20)
    (15 19)
    (14 18)
    (12 15)
    (12 16)
    (13 20)
    (12 17)
    (12 18)
    (12 19)
    (12 20)
    (11 14)
    (10 20)
    (10 19)
    (10 17)
    (9 20)
    (10 16)
    (9 18)
    (8 20)
    (9 17)
    (7 20)
    (8 18)
    (9 16)
    (6 20)
    (8 17)
    (7 18)
    (6 19)
    (5 20)
    (10 14)
    (4 19)
    (5 18)
    (6 17)
    (2 20)
    (3 19)
    (4 18)
    (8 15)
    (0 20)
    (1 19)
    (2 18)
    (9 14)
    (0 18)
    (4 16)
    (6 15)
    (0 17)
    (8 14)
    (1 16)
    (4 15)
    (0 16)
    (7 14)
    (2 15)
    (6 14)
    (0 15)
    (5 14)
    (4 14)
    (3 14)
    (2 14)
    (1 14)
    (0 14)
    (10 13)
    (1 12)
    (3 12)
    (4 12)
    (6 12)
    (2 11)
    (3 11)
    (0 10)
    (4 11)
    (1 10)
    (8 12)
    (0 9)
    (3 10)
    (6 11)
    (4 10)
    (2 9)
    (0 8)
    (7 11)
    (0 7)
    (2 8)
    (4 9)
    (6 10)
    (3 8)
    (2 7)
    (4 8)
    (0 5)
    (7 10)
    (2 6)
    (6 9)
    (0 4)
    (4 7)
    (3 6)
    (2 5)
    (1 4)
    (0 3)
    (10 12)
    (1 2)
    (2 3)
    (3 4)
    (4 5)
    (5 6)
    (1 1)
    (2 2)
    (7 8)
    (5 5)
    (3 2)
    (6 6)
    (4 3)
    (7 7)
    (6 5)
    (3 0)
    (8 8)
    (4 1)
    (7 6)
    (6 4)
    604 |}]
;;
