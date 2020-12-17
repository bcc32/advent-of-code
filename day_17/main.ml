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

let neighbors (x, y, z) =
  let%bind.List dx = [ -1; 0; 1 ] in
  let%bind.List dy = [ -1; 0; 1 ] in
  let%bind.List dz = [ -1; 0; 1 ] in
  if dx = 0 && dy = 0 && dz = 0 then [] else [ x + dx, y + dy, z + dz ]
;;

module Point = struct
  type t = int * int * int [@@deriving compare, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let simulate initial_grid ~n =
  let active_points = Hash_set.create (module Point) in
  for x = 0 to Array.length initial_grid - 1 do
    for y = 0 to Array.length initial_grid.(0) - 1 do
      if initial_grid.(x).(y) then Hash_set.add active_points (x, y, 0)
    done
  done;
  let active_points = ref active_points in
  for _ = 1 to n do
    let new_active_points = Hash_set.create (module Point) in
    let neighbors_of_active_points =
      !active_points
      |> Hash_set.to_list
      |> List.concat_map ~f:neighbors
      |> Set.of_list (module Point)
    in
    let points_to_check =
      (!active_points |> Hash_set.to_list) @ (neighbors_of_active_points |> Set.to_list)
      |> Set.stable_dedup_list (module Point)
    in
    List.iter points_to_check ~f:(fun point ->
      let neighbor_count =
        List.count (neighbors point) ~f:(Hash_set.mem !active_points)
      in
      let should_be_active =
        if Hash_set.mem !active_points point
        then neighbor_count = 2 || neighbor_count = 3
        else neighbor_count = 3
      in
      if should_be_active then Hash_set.add new_active_points point);
    active_points := new_active_points
  done;
  !active_points
;;

let a () =
  let%bind initial_grid = Lazy_deferred.force_exn Input.t in
  let active_points = simulate initial_grid ~n:6 in
  print_s [%sexp (Hash_set.length active_points : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 230 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  print_s [%sexp (input : Input.t)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () =
    [%expect
      {|
        ((true false true true false false false false)
         (false true false true false true true false)
         (true true true false false false false false)
         (false false false false true true false true)
         (true false false false false true true true)
         (false true false true false true false false)
         (false true true false false false true true)
         (true false false true false true true true)) |}]
  in
  return ()
;;
