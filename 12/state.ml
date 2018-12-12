open! Core
open! Async
open! Import

type bool_array = bool array

let sexp_of_bool_array t =
  Array.to_list t
  |> List.map ~f:(fun b -> if b then "#" else ".")
  |> String.concat
  |> Sexp.Atom
;;

type t =
  { plants : bool_array
  ; offset : int
  (* number of pots to the left of 0 *)
  }
[@@deriving sexp_of]

let of_init array = { plants = Array.copy array; offset = 0 }

let step t steps =
  let offset' = t.offset + 2 in
  let plants' =
    let get_current i =
      i + t.offset >= 0
      && i + t.offset < Array.length t.plants
      && t.plants.(i + t.offset)
    in
    Array.init
      (Array.length t.plants + 4)
      ~f:(fun i' ->
        let a = get_current (i' - offset' - 2) in
        let b = get_current (i' - offset' - 1) in
        let c = get_current (i' - offset') in
        let d = get_current (i' - offset' + 1) in
        let e = get_current (i' - offset' + 2) in
        Hashtbl.find_exn steps (a, b, c, d, e))
  in
  { plants = plants'; offset = offset' }
;;

let iteri t ~f =
  let rec loop i =
    if i < Array.length t.plants
    then (
      f (i - t.offset) t.plants.(i);
      loop (i + 1))
  in
  loop 0
;;
