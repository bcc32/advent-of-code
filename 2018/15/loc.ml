open! Core
open! Async
open! Import

module T = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, equal, hash, sexp_of]
end

include T
include Hashable.Make_plain (T)

let is_adjacent { x = x1; y = y1 } { x = x2; y = y2 } =
  (x1 = x2 && Int.abs (y1 - y2) = 1) || (y1 = y2 && Int.abs (x1 - x2) = 1)
;;

let adjacent { x; y } ~height ~width =
  [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
  |> List.filter_map ~f:(fun (x, y) ->
    if 0 <= x && x < height && 0 <= y && y < width then Some { x; y } else None)
;;
