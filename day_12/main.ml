open! Core
open! Async
open! Import

module Dir = struct
  type t =
    | N
    | E
    | S
    | W
  [@@deriving sexp_of]

  let incr t (x, y) ~n =
    match t with
    | N -> x, y + n
    | E -> x + n, y
    | S -> x, y - n
    | W -> x - n, y
  ;;

  let turn_left = function
    | N -> W
    | E -> N
    | S -> E
    | W -> S
  ;;

  let turn_right = function
    | N -> E
    | E -> S
    | S -> W
    | W -> N
  ;;
end

module State = struct
  type t =
    { x : int
    ; y : int
    ; facing : Dir.t
    }
  [@@deriving sexp_of]
end

module Instruction = struct
  type t =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of int
    | Right of int
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
      | 'N' -> North value
      | 'S' -> South value
      | 'E' -> East value
      | 'W' -> West value
      | 'L' -> Left value
      | 'R' -> Right value
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

let quarter_turns_of_degrees n = if n % 90 = 0 then n / 90 else failwith "not valid"

let apply_instruction (state : State.t) (instruction : Instruction.t) : State.t =
  match instruction with
  | North d -> { state with y = state.y + d }
  | South d -> { state with y = state.y - d }
  | East d -> { state with x = state.x + d }
  | West d -> { state with x = state.x - d }
  | Left deg ->
    { state with
      facing =
        Fn.apply_n_times Dir.turn_left ~n:(quarter_turns_of_degrees deg) state.facing
    }
  | Right deg ->
    { state with
      facing =
        Fn.apply_n_times Dir.turn_right ~n:(quarter_turns_of_degrees deg) state.facing
    }
  | Forward n ->
    let x, y = Dir.incr state.facing ~n (state.x, state.y) in
    { state with x; y }
;;

let a () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let state : State.t = { x = 0; y = 0; facing = E } in
  let state = List.fold instructions ~init:state ~f:apply_instruction in
  print_s [%sexp (Int.abs state.x + Int.abs state.y : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 1482 |}] in
  return ()
;;

module State2 = struct
  type t =
    { x : int
    ; y : int
    ; waypoint_x : int
    ; waypoint_y : int
    }
  [@@deriving sexp_of]
end

let rotate_vec_left (x, y) = -y, x
let rotate_vec_right (x, y) = y, -x

let apply_instruction (state : State2.t) (instruction : Instruction.t) : State2.t =
  match instruction with
  | North d -> { state with waypoint_y = state.waypoint_y + d }
  | South d -> { state with waypoint_y = state.waypoint_y - d }
  | East d -> { state with waypoint_x = state.waypoint_x + d }
  | West d -> { state with waypoint_x = state.waypoint_x - d }
  | Left deg ->
    let waypoint_x, waypoint_y =
      Fn.apply_n_times
        rotate_vec_left
        ~n:(quarter_turns_of_degrees deg)
        (state.waypoint_x, state.waypoint_y)
    in
    { state with waypoint_x; waypoint_y }
  | Right deg ->
    let waypoint_x, waypoint_y =
      Fn.apply_n_times
        rotate_vec_right
        ~n:(quarter_turns_of_degrees deg)
        (state.waypoint_x, state.waypoint_y)
    in
    { state with waypoint_x; waypoint_y }
  | Forward n ->
    let x, y = state.x + (n * state.waypoint_x), state.y + (n * state.waypoint_y) in
    { state with x; y }
;;

let b () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let state : State2.t = { x = 0; y = 0; waypoint_x = 10; waypoint_y = 1 } in
  let state = List.fold instructions ~init:state ~f:apply_instruction in
  print_s [%sexp (Int.abs state.x + Int.abs state.y : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 48739 |}] in
  return ()
;;
