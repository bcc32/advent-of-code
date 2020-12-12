open! Core
open! Import

module Cartesian = struct
  type t = [ `Cartesian of Vec2.t ] [@@deriving compare, equal, hash, sexp_of]

  let create ~x ~y : t = `Cartesian { x; y }
  let update (`Cartesian v) ~f = `Cartesian (f v)
  let to_pair (`Cartesian { x; y } : t) = x, y
  let x (`Cartesian { x; y = _ } : t) = x
  let y (`Cartesian { x = _; y } : t) = y
end

module RC = struct
  type t = [ `RC of Vec2.t ] [@@deriving compare, equal, hash, sexp_of]

  let create ~row ~col : t = `RC { x = row; y = col }
  let update (`RC v) ~f = `RC (f v)
  let to_pair (`RC { x = row; y = col } : t) = row, col
  let row (`RC { x = row; y = _ } : t) = row
  let col (`RC { x = _; y = col } : t) = col
end

type t =
  [ Cartesian.t
  | RC.t
  ]
[@@deriving compare, equal, hash, sexp_of]

let cartesian_of_rc (`RC { x = r; y = c } : RC.t) ~width:_ ~height : Cartesian.t =
  `Cartesian { x = height - r - 1; y = c }
;;

let rc_of_cartesian (`Cartesian { x; y } : Cartesian.t) ~width:_ ~height : RC.t =
  `RC { x = height - x - 1; y }
;;

let to_cartesian (t : t) ~width ~height =
  match t with
  | #Cartesian.t as cartesian -> cartesian
  | `RC _ as rc -> cartesian_of_rc rc ~width ~height
;;

let to_rc (t : t) ~width ~height =
  match t with
  | #RC.t as rc -> rc
  | `Cartesian _ as cartesian -> rc_of_cartesian cartesian ~width ~height
;;
