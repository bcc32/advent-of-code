open! Core
open! Async
open! Import

type t =
  | Elf
  | Goblin
[@@deriving equal, sexp_of]

let to_char = function
  | Elf -> 'E'
  | Goblin -> 'G'
;;
