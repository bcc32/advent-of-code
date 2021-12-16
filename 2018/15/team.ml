open! Core
open! Async
open! Import

module T = struct
  type t =
    | Elf
    | Goblin
  [@@deriving bin_io, compare, enumerate, equal, sexp]
end

include T
module Total_map = Total_map.Make (T)

let to_char = function
  | Elf -> 'E'
  | Goblin -> 'G'
;;
