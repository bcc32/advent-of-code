open! Core
open! Import

module T : sig
  type 'a t = private 'a array array [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t

  val width : _ t -> int
  val height : _ t -> int
  val of_matrix_exn : 'a array array -> 'a t
  val get_exn : 'a t -> row:int -> col:int -> 'a
  val set_exn : 'a t -> row:int -> col:int -> 'a -> unit
end = struct
  type 'a t = 'a array array [@@deriving sexp_of]

  let invariant invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      [%test_pred: int] Int.is_positive (Array.length t);
      let width = Array.length t.(0) in
      [%test_pred: int] Int.is_positive width;
      Array.iteri t ~f:(fun i row ->
        [%test_result: int]
          (Array.length row)
          ~expect:width
          ~message:(sprintf "row %d" i));
      Array.iter t ~f:(Array.iter ~f:invariant_a))
  ;;

  let width t = Array.length t.(0)
  let height t = Array.length t

  let of_matrix_exn matrix =
    invariant ignore matrix;
    matrix
  ;;

  let get_exn t ~row ~col = t.(row).(col)
  let set_exn t ~row ~col value = t.(row).(col) <- value
end

include T

let with_dimensions t ~f = f ~width:(width t) ~height:(height t)

let get_row_col t coord =
  with_dimensions t ~f:(Coord2.to_rc (coord :> Coord2.t)) |> Coord2.RC.to_pair
;;

let is_in_bounds t coord =
  let row, col = get_row_col t coord in
  row >= 0 && row < height t && col >= 0 && col < width t
;;

let get_exn t coord =
  let row, col = get_row_col t coord in
  get_exn t ~row ~col
;;

let set_exn t coord value =
  let row, col = get_row_col t coord in
  set_exn t ~row ~col value
;;

module O = struct
  let ( .%() ) = get_exn
  let ( .%()<- ) = set_exn
end
