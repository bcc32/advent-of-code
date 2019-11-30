open! Core
open! Async
open! Import

type t =
  | Elf
  | Goblin
[@@deriving equal, sexp_of]

val to_char : t -> char
