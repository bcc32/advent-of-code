open! Core
open! Async
open! Import

type t =
  { mutable loc : Loc.t
  ; mutable hit_points : int
  ; attack_power : int
  ; team : Team.t
  }
[@@deriving fields, sexp_of]

let create = Fields.create
let is_alive t = t.hit_points > 0

let receive_damage t ~points =
  t.hit_points <- t.hit_points - points;
  if is_alive t then `Alive else `Dead
;;
