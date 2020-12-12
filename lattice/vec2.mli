open! Core
open! Import

type t =
  { x : int
  ; y : int
  }
[@@deriving compare, equal, fields, hash, sexp_of]

include Comparator.S with type t := t
include Comparisons.Infix with type t := t

module O : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val neg : t -> t
  val zero : t
end

include module type of O

val scale : t -> int -> t
