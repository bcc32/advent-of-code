open! Core
open! Async
open! Import

type t =
  { init : bool array
  ; step : (bool * bool * bool * bool * bool, bool) Hashtbl.t
  }
[@@deriving sexp_of]

let read () =
  let%bind contents = Reader.file_contents "aoc.in" in
  let lines = String.split_lines contents in
  let initial_state, lines =
    match lines with
    | hd :: "" :: tl ->
      ( hd
        |> String.subo ~pos:(String.length "initial state: ")
        |> String.to_list
        |> Array.of_list_map ~f:(Char.( = ) '#')
      , tl )
    | _ -> assert false
  in
  let moves =
    List.map lines ~f:(fun line ->
      let open Char.O in
      let a = line.[0] = '#' in
      let b = line.[1] = '#' in
      let c = line.[2] = '#' in
      let d = line.[3] = '#' in
      let e = line.[4] = '#' in
      let result = line.[String.length ".##.. => "] = '#' in
      (a, b, c, d, e), result)
    |> Hashtbl.Poly.of_alist_exn
  in
  return { init = initial_state; step = moves }
;;
