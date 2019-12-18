open! Core
open! Import
module Dir = Dir
module Point = Point

type with_dir
type without_dir
type _ t [@@deriving sexp_of]

val create_without_dir : initial_loc:Point.t -> without_dir t
val create_with_dir : initial_loc:Point.t -> initial_dir:Dir.t -> with_dir t
val loc : _ t -> Point.t
val step_forward : with_dir t -> unit
val step_backward : with_dir t -> unit
val step_dir : _ t -> dir:Dir.t -> unit
val dir : with_dir t -> Dir.t
val turn : with_dir t -> [ `Left | `Right ] -> unit
