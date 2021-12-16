open! Core
open! Async
open! Import

module Input = struct
  open! Advent_of_code_input_helpers

  type t = bool array array [@@deriving sexp_of]

  let parse input = (grid input ~f:(Char.( = ) '#') :> t)

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

module type Point = sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t

  val of_vec2 : int * int -> t
  val neighbors : t -> t list
end

module Point3 : Point with type t = int * int * int = struct
  type t = int * int * int [@@deriving compare, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  let of_vec2 (x, y) = x, y, 0

  let neighbors (x, y, z) =
    let%bind.List dx = [ -1; 0; 1 ] in
    let%bind.List dy = [ -1; 0; 1 ] in
    let%bind.List dz = [ -1; 0; 1 ] in
    if dx = 0 && dy = 0 && dz = 0 then [] else [ x + dx, y + dy, z + dz ]
  ;;
end

let simulate (module Point : Point) initial_grid ~n =
  let active_points = Hash_set.create (module Point) in
  for x = 0 to Array.length initial_grid - 1 do
    for y = 0 to Array.length initial_grid.(0) - 1 do
      if initial_grid.(x).(y) then Hash_set.add active_points (Point.of_vec2 (x, y))
    done
  done;
  let active_points = ref active_points in
  for _ = 1 to n do
    let new_active_points = Hash_set.create (module Point) in
    let neighbors_of_active_points =
      !active_points
      |> Hash_set.to_list
      |> List.concat_map ~f:Point.neighbors
      |> Set.of_list (module Point)
    in
    let points_to_check =
      (!active_points |> Hash_set.to_list) @ (neighbors_of_active_points |> Set.to_list)
      |> Set.stable_dedup_list (module Point)
    in
    List.iter points_to_check ~f:(fun point ->
      let neighbor_count =
        List.count (Point.neighbors point) ~f:(Hash_set.mem !active_points)
      in
      let should_be_active =
        if Hash_set.mem !active_points point
        then neighbor_count = 2 || neighbor_count = 3
        else neighbor_count = 3
      in
      if should_be_active then Hash_set.add new_active_points point);
    active_points := new_active_points
  done;
  Hash_set.length !active_points
;;

let a () =
  let%bind initial_grid = Lazy_deferred.force_exn Input.t in
  let active_points = simulate (module Point3) initial_grid ~n:6 in
  print_s [%sexp (active_points : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 230 |}] in
  return ()
;;

module Point4 = struct
  type t = int * int * int * int [@@deriving compare, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  let of_vec2 (x, y) = x, y, 0, 0

  let neighbors (x, y, z, w) =
    let%bind.List dx = [ -1; 0; 1 ] in
    let%bind.List dy = [ -1; 0; 1 ] in
    let%bind.List dz = [ -1; 0; 1 ] in
    let%bind.List dw = [ -1; 0; 1 ] in
    if dx = 0 && dy = 0 && dz = 0 && dw = 0
    then []
    else [ x + dx, y + dy, z + dz, w + dw ]
  ;;
end

let b () =
  let%bind initial_grid = Lazy_deferred.force_exn Input.t in
  let active_points = simulate (module Point4) initial_grid ~n:6 in
  print_s [%sexp (active_points : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 1600 |}] in
  return ()
;;
