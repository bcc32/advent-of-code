open! Core
open! Async
open! Import
module Dir = Advent_of_code_lattice_geometry.Dir.Four
module Turn = Advent_of_code_lattice_geometry.Turn
module Vec2 = Advent_of_code_lattice_geometry.Vec2

module State1 = struct
  type t =
    { position : Vec2.t
    ; facing : Dir.t
    }
  [@@deriving sexp_of]
end

module Instruction = struct
  type t =
    | Translate of Dir.t * int
    | Rotate of Turn.t * int
    | Forward of int
  [@@deriving sexp_of]

  let of_string =
    let pattern =
      let open Re in
      compile (seq [ group (set "NSEWLRF"); group (rep1 digit) ])
    in
    fun input ->
      let g = Re.exec pattern input in
      let value = Re.Group.get g 2 |> Int.of_string in
      match (Re.Group.get g 1).[0] with
      | 'N' -> Translate (`N, value)
      | 'S' -> Translate (`S, value)
      | 'E' -> Translate (`E, value)
      | 'W' -> Translate (`W, value)
      | 'L' -> Rotate (L, value)
      | 'R' -> Rotate (R, value)
      | 'F' -> Forward value
      | _ -> failwith "invalid direction"
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Instruction.t list [@@deriving sexp_of]

  let parse input : t = lines input |> List.map ~f:Instruction.of_string

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let apply_instruction (state : State1.t) (instruction : Instruction.t) : State1.t =
  let open Vec2.O in
  match instruction with
  | Translate (dir, by) ->
    { state with position = state.position + (by * Dir.unit_vec_cartesian dir) }
  | Rotate (turn, degrees) ->
    { state with facing = Dir.turn_multi_exn state.facing turn ~degrees }
  | Forward n ->
    { state with position = state.position + (n * Dir.unit_vec_cartesian state.facing) }
;;

let a () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let state : State1.t = { position = Vec2.zero; facing = `E } in
  let state = List.fold instructions ~init:state ~f:apply_instruction in
  print_s [%sexp (Int.abs state.position.x + Int.abs state.position.y : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 1482 |}] in
  return ()
;;

module State2 = struct
  type t =
    { position : Vec2.t
    ; waypoint : Vec2.t
    }
  [@@deriving sexp_of]
end

let apply_instruction (state : State2.t) (instruction : Instruction.t) : State2.t =
  let open Vec2.O in
  match instruction with
  | Translate (dir, by) ->
    { state with waypoint = state.waypoint + (by * Dir.unit_vec_cartesian dir) }
  | Rotate (turn, degrees) ->
    let waypoint = Vec2.rotate_wrt_origin_multi_exn state.waypoint turn ~degrees in
    { state with waypoint }
  | Forward n ->
    let position = state.position + (n * state.waypoint) in
    { state with position }
;;

let b () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let state : State2.t = { position = Vec2.zero; waypoint = { x = 10; y = 1 } } in
  let state = List.fold instructions ~init:state ~f:apply_instruction in
  print_s [%sexp (Int.abs state.position.x + Int.abs state.position.y : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 48739 |}] in
  return ()
;;
