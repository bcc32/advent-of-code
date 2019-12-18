open! Core
open! Import
module Dir = Dir
module Point = Point

type with_dir
type without_dir

type 'a t =
  | With_dir :
      { mutable dir : Dir.t
      ; mutable loc : Point.t
      }
      -> with_dir t
  | Without_dir : { mutable loc : Point.t } -> without_dir t
[@@deriving sexp_of]

let create_without_dir ~initial_loc:loc = Without_dir { loc }
let create_with_dir ~initial_loc:loc ~initial_dir:dir = With_dir { dir; loc }

let loc : type a. a t -> Point.t = function
  | With_dir { dir = _; loc } | Without_dir { loc } -> loc
;;

let step_forward (With_dir t : with_dir t) = t.loc <- Point.add t.loc t.dir
let step_backward (With_dir t : with_dir t) = t.loc <- Point.add t.loc (Dir.opp t.dir)

let step_dir (type a) (t : a t) ~dir =
  match t with
  | With_dir t -> t.loc <- Point.add t.loc dir
  | Without_dir t -> t.loc <- Point.add t.loc dir
;;

let dir (With_dir t : with_dir t) = t.dir
let turn (With_dir t : with_dir t) which_way = t.dir <- Dir.turn t.dir which_way
