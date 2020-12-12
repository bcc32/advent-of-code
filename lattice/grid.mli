open! Core
open! Import

(** Represents a grid of elements in row-major order.  Canonically, when
    considered as a grid in 2D space on a sheet of paper, the first elements of
    the outer array are the uppermost rows of the grid. *)
type 'a t = private 'a array array [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** Raises if either dimension is zero or the rows do not all have the same
    length. *)
val of_matrix_exn : 'a array array -> 'a t

val width : _ t -> int
val height : _ t -> int
val is_in_bounds : _ t -> [< Coord2.t ] -> bool
val get_exn : 'a t -> [< Coord2.t ] -> 'a
val set_exn : 'a t -> [< Coord2.t ] -> 'a -> unit

(** Convenience function to pass dimensions as labeled arguments to a
    continuation. *)
val with_dimensions : 'a t -> f:(width:int -> height:int -> 'a) -> 'a

module O : sig
  (** Same as [get_exn]. *)
  val ( .%() ) : 'a t -> [< Coord2.t ] -> 'a

  (** Same as [set_exn]. *)
  val ( .%()<- ) : 'a t -> [< Coord2.t ] -> 'a -> unit
end
