open! Core
open! Import

type t =
  { x : int
  ; y : int
  }
[@@deriving compare, equal, fields, hash, sexp_of]

include (val Comparator.make ~compare ~sexp_of_t)

module Comparable_infix = Comparable.Infix (struct
    type nonrec t = t [@@deriving compare]
  end)

let scale t n = { x = t.x * n; y = t.y * n }

module O = struct
  let ( + ) t1 t2 = { x = t1.x + t2.x; y = t1.y + t2.y }
  let ( - ) t1 t2 = { x = t1.x - t2.x; y = t1.y - t2.y }
  let ( * ) = Fn.flip scale
  let zero = { x = 0; y = 0 }
  let neg t = { x = -t.x; y = -t.y }
  let ( ~- ) = neg

  include Comparable_infix
end

let rotate_wrt_origin { x; y } : Turn.t -> t = function
  | L -> { x = -y; y = x }
  | R -> { x = y; y = -x }
;;

let rotate_wrt_origin_multi_exn t (turn : Turn.t) ~degrees =
  let degrees =
    match turn with
    | L -> -degrees
    | R -> degrees
  in
  let degrees = degrees % 360 in
  if degrees % 90 <> 0
  then raise_s [%message "Cannot rotate lattice vector" (degrees : int)]
  else Fn.apply_n_times ~n:(degrees / 90) (Fn.flip rotate_wrt_origin R) t
;;

include O
