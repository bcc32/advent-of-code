open! Core
open! Async
open! Import

type t = private
  { mutable memory : int array
  ; mutable relative_base : int
  ; mutable pc : int
  ; mutable input : int Queue.t
  }
[@@deriving sexp_of]

val of_string : string -> t
val copy : t -> t

module Sync : sig
  module Step_result : sig
    type t =
      | Done
      | Need_input
      | Output of int
  end

  val run_without_input_exn : t -> f:(int -> unit) -> unit
  val step : t -> Step_result.t
  val provide_input : t -> int -> unit
  val provide_input' : t -> int Queue.t -> unit
end

module Async : sig
  module Run : sig
    type t = private
      { input : int Pipe.Writer.t
      ; output : int Pipe.Reader.t
      ; done_ : unit Deferred.t
      }
  end

  val run : t -> Run.t
end
