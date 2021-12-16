open! Core
open! Import

(** Represents a robot, elf, or other object capable of navigating around a 2D
    integer grid.  Has a position and a direction. *)
type 'dir_type t [@@deriving sexp_of]
