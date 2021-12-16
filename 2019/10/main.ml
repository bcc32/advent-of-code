open! Core
open! Async
open! Import

let debug = false

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> Array.of_list_map ~f:String.to_array
;;

module Coord : sig
  type t = int * int [@@deriving compare, sexp_of]
  type coord := t

  include Comparable.S_plain with type t := t

  val dist : t -> t -> float
  val all : 'a array array -> t Sequence.t
  val get : 'a array array -> t -> 'a

  module Dir : sig
    type t [@@deriving compare, equal, sexp_of]

    include Comparable.S_plain with type t := t

    val vec : from:coord -> to_:coord -> t
  end
end = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T

  let dist (x, y) (x', y') =
    let dx = float (x' - x) in
    let dy = float (y - y') in
    Float.hypot dx dy
  ;;

  let all grid =
    Sequence.cartesian_product
      (Sequence.range 0 (Array.length grid.(0)))
      (Sequence.range 0 (Array.length grid))
  ;;

  let get grid (x, y) = grid.(y).(x)

  module Dir = struct
    module T = struct
      type t =
        { vec : int * int
        ; theta_from_north : float [@equal.ignore] [@hash.ignore]
        }
      [@@deriving equal, sexp_of]

      let compare t u =
        if equal t u then 0 else Float.compare t.theta_from_north u.theta_from_north
      ;;
    end

    include T

    let rec gcd a b = if b = 0 then a else gcd b (a % b)

    let theta_from_north (dx, dy) =
      let open Float.O in
      let theta = Float.atan2 (float dy) (float dx) in
      let theta_from_north = (Float.pi / 2.) - theta in
      if theta_from_north < 0.
      then theta_from_north + (Float.pi * 2.)
      else theta_from_north
    ;;

    let vec ~from:(x, y) ~to_:(x', y') =
      (* y axis is inverted. *)
      let dx = x' - x in
      let dy = y - y' in
      let g = gcd (Int.abs dx) (Int.abs dy) in
      let vec = dx / g, dy / g in
      { vec; theta_from_north = theta_from_north vec }
    ;;

    include Comparable.Make_plain (T)
  end

  include Comparable.Make_plain (T)
end

let count_visible_asteroids grid ~from =
  Sequence.filter_map (Coord.all grid) ~f:(fun coord ->
    if Coord.( <> ) from coord && Char.( = ) (Coord.get grid coord) '#'
    then Some (Coord.Dir.vec ~from ~to_:coord)
    else None)
  |> Sequence.to_list
  |> Coord.Dir.Set.of_list
  |> Set.length
;;

let best_station_coordinates grid =
  Coord.all grid
  |> Sequence.filter ~f:(fun c -> Char.( = ) (Coord.get grid c) '#')
  |> Sequence.max_elt
       ~compare:
         (Comparable.lift [%compare: int] ~f:(fun v ->
            count_visible_asteroids grid ~from:v))
  |> Option.value_exn
;;

let a () =
  let%bind grid = input () in
  let v = best_station_coordinates grid in
  if debug then print_s [%sexp (v : Coord.t)];
  count_visible_asteroids grid ~from:v |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 227 |}]
;;

let all_asteroids grid =
  Coord.all grid |> Sequence.filter ~f:(fun c -> Char.( = ) (Coord.get grid c) '#')
;;

let b () =
  let%bind grid = input () in
  let v = best_station_coordinates grid in
  let asteroids_in_laser_order =
    all_asteroids grid
    |> Sequence.filter ~f:(Coord.( <> ) v)
    |> Sequence.map ~f:(fun p -> Coord.Dir.vec ~from:v ~to_:p, p)
    |> Coord.Dir.Map.of_sequence_multi
    |> Map.map
         ~f:(List.sort ~compare:(Comparable.lift [%compare: float] ~f:(Coord.dist v)))
    |> Map.data
    |> List.map ~f:Sequence.of_list
    |> Sequence.round_robin
  in
  if debug
  then print_s [%sexp (Sequence.take asteroids_in_laser_order 200 : Coord.t Sequence.t)];
  let x, y = Sequence.nth_exn asteroids_in_laser_order 199 in
  printf "%d\n" ((100 * x) + y);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 604 |}]
;;
