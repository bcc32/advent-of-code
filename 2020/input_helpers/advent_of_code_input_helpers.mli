open! Core
open! Async
open! Import

val stoi : string -> int
val grid : string -> f:(char -> 'a) -> 'a Advent_of_code_lattice_geometry.Grid.t
val words : ?sep:string -> string -> string list
val lines : string -> string list
val paragraphs : string list -> string list list
