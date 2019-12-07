open! Core
open! Async
open! Import

type t = int array [@@deriving sexp_of]

val of_string : string -> t
val copy : t -> t
val run : t -> input:int Pipe.Reader.t -> output:int Pipe.Writer.t -> unit Deferred.t
val run_without_io : t -> unit Deferred.t
