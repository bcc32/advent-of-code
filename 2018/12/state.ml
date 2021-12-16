open! Core
open! Async
open! Import

type t = Int.Set.t [@@deriving sexp_of]

let of_init array =
  Array.filter_mapi array ~f:(fun i p -> if p then Some i else None) |> Int.Set.of_array
;;

let step t steps =
  t
  |> Set.to_list
  |> List.concat_map ~f:(fun x -> [ x - 2; x - 1; x; x + 1; x + 2 ])
  |> Int.Set.of_list
  |> Set.filter ~f:(fun x ->
    let a = Set.mem t (x - 2) in
    let b = Set.mem t (x - 1) in
    let c = Set.mem t x in
    let d = Set.mem t (x + 1) in
    let e = Set.mem t (x + 2) in
    Hashtbl.find_exn steps (a, b, c, d, e))
;;

let sum_plants t = Set.sum (module Int) t ~f:Fn.id
let count_plants = Set.length
