open! Core
open! Import

type t = int * int [@@deriving compare, hash, sexp_of]

let dist_manhattan (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)
let dist_euclidean (x1, y1) (x2, y2) = Float.hypot (float (x1 - x2)) (float (y1 - y2))

let add (x, y) (dir : Dir.t) =
  match dir with
  | N -> x, y + 1
  | E -> x + 1, y
  | S -> x, y - 1
  | W -> x - 1, y
;;
