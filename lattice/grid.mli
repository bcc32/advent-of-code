open! Core
open! Import

(** Represents a grid of elements in row-major order.  Canonically, when
    considered as a grid in 2D space on a sheet of paper, the first elements of
    the outer array are the uppermost rows of the grid. *)
type 'a t = private 'a array array [@@deriving compare, equal, sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** Raises if either dimension is zero or the rows do not all have the same
    length. *)
val of_matrix_exn : 'a array array -> 'a t

val width : _ t -> int
val height : _ t -> int
val is_in_bounds : _ t -> [< Coord.t ] -> bool
val get_exn : 'a t -> [< Coord.t ] -> 'a
val set_exn : 'a t -> [< Coord.t ] -> 'a -> unit

(** Convenience function to pass dimensions as labeled arguments to a
    continuation. *)
val with_dimensions : 'a t -> f:(width:int -> height:int -> 'a) -> 'a

val copy : 'a t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t

module O : sig
  (** Same as [get_exn]. *)
  val ( .%() ) : 'a t -> [< Coord.t ] -> 'a

  (** Same as [set_exn]. *)
  val ( .%()<- ) : 'a t -> [< Coord.t ] -> 'a -> unit

  (** Same as [is_in_bounds]. *)
  val ( .?() ) : _ t -> [< Coord.t ] -> bool
end

module Lines : sig
  (** Each function takes the grid, a major direction of travel (across lines),
      and a minor direction of travel (within lines).  It then returns a sequence
      of lines of coordinates within the grid. *)

  val rows
    :  _ t
    -> [ `Top_to_bottom | `Bottom_to_top ]
    -> [ `Left_to_right | `Right_to_left ]
    -> Coord.RC.t Sequence.t Sequence.t

  val cols
    :  _ t
    -> [ `Left_to_right | `Right_to_left ]
    -> [ `Top_to_bottom | `Bottom_to_top ]
    -> Coord.RC.t Sequence.t Sequence.t

  val backslash_diagonals
    :  _ t
    -> [ `Top_right_to_bottom_left | `Bottom_left_to_top_right ]
    -> [ `Top_left_to_bottom_right | `Bottom_right_to_top_left ]
    -> Coord.RC.t Sequence.t Sequence.t

  val forward_slash_diagonals
    :  _ t
    -> [ `Top_left_to_bottom_right | `Bottom_right_to_top_left ]
    -> [ `Top_right_to_bottom_left | `Bottom_left_to_top_right ]
    -> Coord.RC.t Sequence.t Sequence.t
end

module Row_major : sig
  include Container.S1 with type 'a t := 'a t
end
