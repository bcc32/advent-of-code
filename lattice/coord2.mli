(** The polymorphic variant constructors of the types in this module are only
    exposed for the purpose of exposing the subtyping relationship.  Do not
    pattern-match on them. *)

open! Core
open! Import

type t =
  [ `Cartesian of Vec2.t
  | `RC of Vec2.t
  ]
[@@deriving sexp_of]

module Cartesian : sig
  (** Cartesian coordinates (x, y).

      (0, 0) denotes the bottom-left corner of the grid.

      x increases to the right.
      y increases to the top. *)
  type t = private [ `Cartesian of Vec2.t ] [@@deriving sexp_of]

  val create : x:int -> y:int -> t
  val to_pair : t -> int * int
  val x : t -> int
  val y : t -> int
end

module RC : sig
  (** Row-column coordinates (r, c).

      (0, 0) denotes the top-left corner of the grid.

      r increases to the bottom.
      c increases to the right. *)
  type t = private [ `RC of Vec2.t ] [@@deriving sexp_of]

  val create : row:int -> col:int -> t
  val to_pair : t -> int * int
  val row : t -> int
  val col : t -> int
end

val cartesian_of_rc : RC.t -> width:int -> height:int -> Cartesian.t
val rc_of_cartesian : Cartesian.t -> width:int -> height:int -> RC.t
val to_cartesian : t -> width:int -> height:int -> Cartesian.t
val to_rc : t -> width:int -> height:int -> RC.t
