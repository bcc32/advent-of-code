open! Core
open! Import

type t =
  { x : int
  ; y : int
  }
[@@deriving compare, equal, fields, hash, sexp_of]

include (val Comparator.make ~compare ~sexp_of_t)

include Comparable.Infix (struct
    type nonrec t = t [@@deriving compare]
  end)

let ( + ) t1 t2 = { x = t1.x + t2.x; y = t1.y + t2.y }
let ( - ) t1 t2 = { x = t1.x - t2.x; y = t1.y - t2.y }
let zero = { x = 0; y = 0 }
let neg t = { x = -t.x; y = -t.y }
let ( ~- ) = neg

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( ~- ) = ( ~- )
  let neg = neg
  let zero = zero
end

let scale t n = { x = t.x * n; y = t.y * n }
