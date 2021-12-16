open! Core
open! Import

type t =
  | N
  | E
  | S
  | W
[@@deriving enumerate, equal, sexp_of]

val opp : t -> t
val turn : t -> [ `Left | `Right ] -> t
val of_char_urdl_exn : char -> t
