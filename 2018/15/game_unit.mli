open! Core
open! Async
open! Import

type t = private
  { mutable loc : Loc.t
  ; mutable hit_points : int
  ; attack_power : int
  ; team : Team.t
  }
[@@deriving fields, sexp_of]

val create : loc:Loc.t -> hit_points:int -> attack_power:int -> team:Team.t -> t
val set_loc : t -> Loc.t -> unit
val receive_damage : t -> points:int -> [ `Dead | `Alive ]
val is_alive : t -> bool
