open! Core
open! Async
open! Import

let stoi = Int.of_string
let lines = String.split_lines

let paragraphs =
  List.group ~break:(fun _ s -> String.is_empty s)
  >> List.map ~f:(List.filter ~f:(not << String.is_empty))
;;

let words ?sep input =
  let pattern =
    let open Re in
    compile
      (match sep with
       | None -> rep1 space
       | Some sep -> str sep)
  in
  Re.split pattern input
;;

let grid ~f =
  lines
  >> Array.of_list_map ~f:(String.to_list_rev >> Array.of_list_rev_map ~f)
  >> Advent_of_code_lattice_geometry.Grid.of_matrix_exn
;;

(* Eta-expand to fix labeled argument order. *)
let grid input ~f = grid input ~f
