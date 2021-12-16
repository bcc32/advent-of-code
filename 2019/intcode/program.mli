open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

val of_string : string -> t

(** {2 Copying program states} *)

module Snapshot : sig
  type program := t
  type t [@@deriving sexp_of]

  val instantiate : t -> program
end

val snapshot : t -> Snapshot.t
val restore : t -> from:Snapshot.t -> unit

(** {2 Accessing memory} *)

module Infix : sig
  (** [t.$(i)] gets the [i]th value in program memory. *)
  val ( .$() ) : t -> int -> int

  (** [t.$(i) <- x] sets the [i]th value in program memory to [x]. *)
  val ( .$()<- ) : t -> int -> int -> unit
end

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

  (** [provide_input' t ~from] transfers input from [from] to [t]'s input queue.

      Removes elements from queue. *)
  val provide_input' : t -> from:int Queue.t -> unit
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
