open! Core
open! Import

type t =
  | N
  | E
  | S
  | W
[@@deriving enumerate, sexp_of]

val turn : t -> [ `Left | `Right ] -> t
val of_char_urdl_exn : char -> t
