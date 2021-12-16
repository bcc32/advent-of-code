open! Core
open! Async
open! Import

exception End_of_combat of { winning_team : Team.t }

type t [@@deriving sexp_of]

val create : ?elf_attack_power:int (** default = 3 *) -> string list -> t
val to_string_hum : t -> string

(** raises {!End_of_combat} if a unit cannot find a target *)
val perform_round : t -> unit

val kill_count : t -> Team.t -> int
val sum_of_hit_points : t -> int
