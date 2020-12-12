open! Core
open! Async
open! Import

let lines = String.split_lines

let paragraphs =
  List.group ~break:(fun _ s -> String.is_empty s)
  >> List.map ~f:(List.filter ~f:(not << String.is_empty))
;;

let words =
  let pattern =
    let open Re in
    compile (rep1 space)
  in
  Re.split pattern
;;

let grid ~f =
  lines >> Array.of_list_map ~f:(String.to_list_rev >> Array.of_list_rev_map ~f)
;;

(* Eta-expand to fix labeled argument order. *)
let grid input ~f = grid input ~f
