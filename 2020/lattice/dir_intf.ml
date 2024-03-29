open! Core
open! Import

type nesw =
  [ `N
  | `E
  | `S
  | `W
  ]
[@@deriving sexp_of]

type diag =
  [ `NE
  | `SE
  | `SW
  | `NW
  ]
[@@deriving sexp_of]

module type S = sig
  (** Represents a compass direction. *)
  type _ t [@@deriving sexp_of]

  (** Represents a single grid square movement.  Might be diagonal, in which
      case the distance is actually sqrt(2) but it's still "one square over".*)
  val unit_vec_cartesian : 'a t -> Vec.t

  val turn : 'a t -> Turn.t -> 'a t
  val turn_left : 'a t -> 'a t
  val turn_right : 'a t -> 'a t
  val turn_multi_exn : 'a t -> Turn.t -> degrees:int -> 'a t
end

module type S0 = sig
  type t [@@deriving sexp_of]

  val unit_vec_cartesian : t -> Vec.t
  val turn : t -> Turn.t -> t
  val turn_left : t -> t
  val turn_right : t -> t
  val turn_multi_exn : t -> Turn.t -> degrees:int -> t
end

module type Dir = sig
  module Four : S0 with type t = nesw

  module Eight :
    S0
    with type t =
      [ nesw
      | diag
      ]

  type 'dir_type t =
    | Four : Four.t -> Four.t t
    | Eight : Eight.t -> Eight.t t

  val generalize : _ t -> Eight.t t

  include S with type 'dir_type t := 'dir_type t
end

(** Check that S and S0 are equivalent. *)
open struct
  module Check_S_satisfies_S0 (M : S) : S0 with type t := unit M.t = struct
    include M

    type t = unit M.t [@@deriving sexp_of]
  end

  module Check_S0_satisfies_S (M : S0) : S with type _ t := M.t = struct
    include M

    type _ t = M.t [@@deriving sexp_of]
  end
end
