open! Core
open! Async
open! Import

module Run : sig
  type t = private
    { input : int Pipe.Writer.t
    ; output : int Pipe.Reader.t
    ; done_ : unit Deferred.t
    }
end

type t = private
  { mutable memory : int array
  ; mutable relative_base : int
  }
[@@deriving sexp_of]

val of_string : string -> t
val copy : t -> t
val run : t -> Run.t
val run_without_io : t -> unit Deferred.t
val run' : t -> input:(int, [> read ]) Mvar.t -> int Pipe.Reader.t
