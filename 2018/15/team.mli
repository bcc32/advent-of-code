open! Core
open! Async
open! Import

type t =
  | Elf
  | Goblin
[@@deriving enumerate, equal, sexp_of]

val to_char : t -> char

module Total_map : Total_map.S with type Key.t = t
