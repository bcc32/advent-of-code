open! Core
open! Async
open! Import

type t

val of_string : string -> t

(** list of (guard id, (sleep, wake) times) *)
val analyze : t list -> (int * (int * int)) list
