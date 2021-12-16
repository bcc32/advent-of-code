open! Core
open! Import
module Dir = Dir
module Point = Point

type _ t [@@deriving sexp_of]

val create_without_dir : initial_loc:Point.t -> [ `dummy ] t
val create_with_dir : initial_loc:Point.t -> initial_dir:Dir.t -> [ `dir ] t
val loc : _ t -> Point.t
val step_forward : [> `dir ] t -> unit
val step_backward : [> `dir ] t -> unit
val step_dir : _ t -> dir:Dir.t -> unit
val dir : [> `dir ] t -> Dir.t
val turn : [> `dir ] t -> [ `Left | `Right ] -> unit
