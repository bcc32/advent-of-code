open! Core
open! Import
include Dir_intf

let turn t turn ~turn_left ~turn_right =
  match turn with
  | L -> turn_left t
  | R -> turn_right t
;;

let turn_clockwise_multi t ~degrees ~turn_right ~single_turn_angle =
  let degrees = degrees % 360 in
  if degrees % single_turn_angle <> 0
  then raise_s [%message "Invalid turn angle" (degrees : int) (single_turn_angle : int)]
  else (
    let turns = degrees / single_turn_angle in
    Fn.apply_n_times ~n:turns turn_right t)
;;

module Four = struct
  type t = nesw [@@deriving sexp_of]

  let turn_left = function
    | `N -> `W
    | `E -> `N
    | `S -> `E
    | `W -> `S
  ;;

  let turn_right = function
    | `N -> `E
    | `E -> `S
    | `S -> `W
    | `W -> `N
  ;;

  let turn = turn ~turn_left ~turn_right
  let turn_clockwise_multi = turn_clockwise_multi ~turn_right ~single_turn_angle:90
end

module Eight = struct
  type t =
    [ nesw
    | diag
    ]
  [@@deriving sexp_of]

  let turn_left = function
    | `N -> `NW
    | `NE -> `N
    | `E -> `NE
    | `SE -> `E
    | `S -> `SE
    | `SW -> `S
    | `W -> `SW
    | `NW -> `W
  ;;

  let turn_right = function
    | `N -> `NE
    | `NE -> `E
    | `E -> `SE
    | `SE -> `S
    | `S -> `SW
    | `SW -> `W
    | `W -> `NW
    | `NW -> `N
  ;;

  let turn = turn ~turn_left ~turn_right
  let turn_clockwise_multi = turn_clockwise_multi ~turn_right ~single_turn_angle:45
end

type _ t =
  | Four : Four.t -> Four.t t
  | Eight : Eight.t -> Eight.t t
[@@deriving sexp_of]

let generalize (type a) (t : a t) =
  let eight =
    match t with
    | Four four -> (four :> Eight.t)
    | Eight eight -> eight
  in
  Eight eight
;;

let dispatch (type a) (t : a t) : (module S0 with type t = a) * a * (a -> a t) =
  match t with
  | Four four -> (module Four), four, fun four -> Four four
  | Eight eight -> (module Eight), eight, fun eight -> Eight eight
;;

let turn_left (type a) (t : a t) =
  let (module M), dir, make = dispatch t in
  M.turn_left dir |> make
;;

let turn_right (type a) (t : a t) =
  let (module M), dir, make = dispatch t in
  M.turn_right dir |> make
;;

let turn (type a) (t : a t) turn =
  let (module M), dir, make = dispatch t in
  M.turn dir turn |> make
;;

let turn_clockwise_multi (type a) (t : a t) ~degrees =
  let (module M), dir, make = dispatch t in
  M.turn_clockwise_multi dir ~degrees |> make
;;
