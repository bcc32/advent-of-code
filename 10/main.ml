open! Core
open! Async
open! Import

let debug = false

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> Array.of_list_map ~f:String.to_array
;;

module Coord : sig
  type t = int * int [@@deriving compare, hash, sexp_of]
  type coord := t

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val dist : t -> t -> float

  module Dir : sig
    type t [@@deriving compare, equal, hash, sexp_of]

    include Comparable.S_plain with type t := t

    val vec : from:coord -> to_:coord -> t
  end
end = struct
  module Dir = struct
    module T = struct
      type t =
        { vec : int * int
        ; theta_from_north : float [@equal.ignore] [@hash.ignore]
        }
      [@@deriving equal, hash, sexp_of]

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
      let dx = x' - x in
      let dy = y - y' in
      let g = gcd (Int.abs dx) (Int.abs dy) in
      let vec = dx / g, dy / g in
      { vec; theta_from_north = theta_from_north vec }
    ;;

    include Comparable.Make_plain (T)
  end

  module T = struct
    type t = int * int [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let dist (x, y) (x', y') =
    let dx = float (x' - x) in
    let dy = float (y - y') in
    Float.hypot dx dy
  ;;
end

let scan grid ~from =
  let scanned_vecs = Hash_set.create (module Coord.Dir) in
  for x' = 0 to Array.length grid.(0) - 1 do
    for y' = 0 to Array.length grid - 1 do
      if Char.( = ) grid.(y').(x') '#'
      then (
        try Hash_set.add scanned_vecs (Coord.Dir.vec ~from ~to_:(x', y')) with
        | _ -> ())
    done
  done;
  Hash_set.length scanned_vecs
;;

let best_station_coordinates grid =
  Sequence.range 0 (Array.length grid.(0))
  |> Sequence.cartesian_product (Sequence.range 0 (Array.length grid))
  |> Sequence.filter ~f:(fun (x, y) -> Char.( = ) grid.(y).(x) '#')
  |> Sequence.max_elt
       ~compare:(Comparable.lift [%compare: int] ~f:(fun v -> scan grid ~from:v))
  |> uw
;;

let a () =
  let%bind grid = input () in
  let v = best_station_coordinates grid in
  if debug then print_s [%sexp (v : Coord.t)];
  scan grid ~from:v |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 227 |}]
;;

let all_asteroids grid =
  Sequence.range 0 (Array.length grid.(0))
  |> Sequence.cartesian_product (Sequence.range 0 (Array.length grid))
  |> Sequence.filter ~f:(fun (x, y) -> Char.( = ) grid.(y).(x) '#')
  |> Sequence.to_list
;;

let b () =
  let%bind grid = input () in
  let v = best_station_coordinates grid in
  let asteroids_in_laser_order =
    all_asteroids grid
    |> List.filter ~f:(Coord.( <> ) v)
    |> List.map ~f:(fun p -> Coord.Dir.vec ~from:v ~to_:p, p)
    |> Coord.Dir.Map.of_alist_multi
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
