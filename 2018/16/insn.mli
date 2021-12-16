open! Core
open! Async
open! Import

module Opcode : sig
  type t =
    | Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
  [@@deriving compare, enumerate, sexp_of]
end

type t =
  { opcode : int
  ; a : int
  ; b : int
  ; c : int
  }

val of_string : string -> t
val exec : t -> memory:int array -> op:(int -> Opcode.t) -> unit
