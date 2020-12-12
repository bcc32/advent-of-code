open! Core
open! Import

module T : sig
  type 'a t = private 'a array array [@@deriving compare, equal, sexp_of]

  include Invariant.S1 with type 'a t := 'a t

  val of_matrix_exn : 'a array array -> 'a t
  val width : _ t -> int
  val height : _ t -> int
  val is_in_bounds_internal : _ t -> row:int -> col:int -> bool
  val get_internal_exn : 'a t -> row:int -> col:int -> 'a
  val set_internal_exn : 'a t -> row:int -> col:int -> 'a -> unit
  val copy : 'a t -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end = struct
  type 'a t = 'a array array [@@deriving compare, equal, sexp_of]

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

  let is_in_bounds_internal t ~row ~col =
    row >= 0 && row < height t && col >= 0 && col < width t
  ;;

  let[@cold] raise_index_out_of_bounds t ~row ~col =
    let height = height t in
    let width = width t in
    raise_s
      [%message
        "Invalid coordinates" (row : int) (height : int) (col : int) (width : int)]
  ;;

  let[@inline always] get_internal_exn t ~row ~col =
    if is_in_bounds_internal t ~row ~col
    then Array.unsafe_get (Array.unsafe_get t row) col
    else raise_index_out_of_bounds t ~row ~col
  ;;

  let[@inline always] set_internal_exn t ~row ~col value =
    if is_in_bounds_internal t ~row ~col
    then Array.unsafe_set (Array.unsafe_get t row) col value
    else raise_index_out_of_bounds t ~row ~col
  ;;

  let copy = Array.map ~f:Array.copy
  let map t ~f = Array.map t ~f:(Array.map ~f)
end

include T

let with_dimensions t ~f = f ~width:(width t) ~height:(height t)

let get_row_col t coord =
  with_dimensions t ~f:(Coord.to_rc (coord :> Coord.t)) |> Coord.RC.to_pair
;;

let is_in_bounds t coord =
  let row, col = get_row_col t coord in
  is_in_bounds_internal t ~row ~col
;;

let get_exn t coord =
  let row, col = get_row_col t coord in
  get_internal_exn t ~row ~col
;;

let set_exn t coord value =
  let row, col = get_row_col t coord in
  set_internal_exn t ~row ~col value
;;

module O = struct
  let ( .%() ) = get_exn
  let ( .%()<- ) = set_exn
  let ( .?() ) = is_in_bounds
end

module Lines = struct
  let iota ?(lbound = 0) ubound ~direction =
    match direction with
    | `Ascending ->
      Sequence.range lbound ubound ~start:`inclusive ~stop:`exclusive ~stride:1
    | `Descending ->
      Sequence.range ubound lbound ~start:`exclusive ~stop:`inclusive ~stride:(-1)
  ;;

  let rows t major minor =
    let%map.Sequence row =
      iota
        (height t)
        ~direction:
          (match major with
           | `Top_to_bottom -> `Ascending
           | `Bottom_to_top -> `Descending)
    in
    let%map.Sequence col =
      iota
        (width t)
        ~direction:
          (match minor with
           | `Left_to_right -> `Ascending
           | `Right_to_left -> `Descending)
    in
    Coord.RC.create ~row ~col
  ;;

  let cols t major minor =
    let%map.Sequence col =
      iota
        (width t)
        ~direction:
          (match major with
           | `Left_to_right -> `Ascending
           | `Right_to_left -> `Descending)
    in
    let%map.Sequence row =
      iota
        (height t)
        ~direction:
          (match minor with
           | `Top_to_bottom -> `Ascending
           | `Bottom_to_top -> `Descending)
    in
    Coord.RC.create ~row ~col
  ;;

  let backslash_diagonals t major minor =
    let including_left_edge_and_top_left_corner =
      let%map.Sequence starting_row =
        iota
          (height t)
          ~direction:
            (match major with
             | `Top_right_to_bottom_left -> `Ascending
             | `Bottom_left_to_top_right -> `Descending)
      in
      let%map.Sequence offset =
        iota
          (Int.min (width t) (height t - starting_row))
          ~direction:
            (match minor with
             | `Top_left_to_bottom_right -> `Ascending
             | `Bottom_right_to_top_left -> `Descending)
      in
      let row, col = starting_row + offset, offset in
      Coord.RC.create ~row ~col
    in
    let including_top_edge =
      let%map.Sequence starting_col =
        iota
          ~lbound:1
          (width t)
          ~direction:
            (match major with
             | `Bottom_left_to_top_right -> `Ascending
             | `Top_right_to_bottom_left -> `Descending)
      in
      let%map.Sequence offset =
        iota
          (Int.min (height t) (width t - starting_col))
          ~direction:
            (match minor with
             | `Top_left_to_bottom_right -> `Ascending
             | `Bottom_right_to_top_left -> `Descending)
      in
      let row, col = offset, starting_col + offset in
      Coord.RC.create ~row ~col
    in
    match major with
    | `Top_right_to_bottom_left ->
      Sequence.append including_top_edge including_left_edge_and_top_left_corner
    | `Bottom_left_to_top_right ->
      Sequence.append including_left_edge_and_top_left_corner including_top_edge
  ;;

  let forward_slash_diagonals t major minor =
    let including_right_edge_and_top_right_corner =
      let%map.Sequence starting_row =
        iota
          (height t)
          ~direction:
            (match major with
             | `Top_left_to_bottom_right -> `Ascending
             | `Bottom_right_to_top_left -> `Descending)
      in
      let%map.Sequence offset =
        iota
          (Int.min (width t) (height t - starting_row))
          ~direction:
            (match minor with
             | `Top_right_to_bottom_left -> `Ascending
             | `Bottom_left_to_top_right -> `Descending)
      in
      let row, col = starting_row + offset, width t - 1 - offset in
      Coord.RC.create ~row ~col
    in
    let including_top_edge =
      let%map.Sequence starting_col =
        iota
          (width t - 1)
          ~direction:
            (match major with
             | `Top_left_to_bottom_right -> `Ascending
             | `Bottom_right_to_top_left -> `Descending)
      in
      let%map.Sequence offset =
        iota
          (Int.min (height t) (starting_col + 1))
          ~direction:
            (match minor with
             | `Bottom_left_to_top_right -> `Ascending
             | `Top_right_to_bottom_left -> `Descending)
      in
      let row, col = offset, starting_col - offset in
      Coord.RC.create ~row ~col
    in
    match major with
    | `Top_left_to_bottom_right ->
      Sequence.append including_top_edge including_right_edge_and_top_right_corner
    | `Bottom_right_to_top_left ->
      Sequence.append including_right_edge_and_top_right_corner including_top_edge
  ;;
end

module Row_major = Container.Make (struct
    type nonrec 'a t = 'a t

    let fold t ~init ~f =
      let accum = ref init in
      for row = 0 to height t - 1 do
        for col = 0 to width t - 1 do
          accum := f !accum (get_internal_exn t ~row ~col)
        done
      done;
      !accum
    ;;

    let iter t ~f =
      for row = 0 to height t - 1 do
        for col = 0 to width t - 1 do
          f (get_internal_exn t ~row ~col)
        done
      done
    ;;

    let iter = `Custom iter
    let length t = height t * width t
    let length = `Custom length
  end)
