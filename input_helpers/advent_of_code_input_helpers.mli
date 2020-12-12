open! Core
open! Async
open! Import

val grid : string -> f:(char -> 'a) -> 'a array array
val words : string -> string list
val lines : string -> string list
val paragraphs : string list -> string list list
