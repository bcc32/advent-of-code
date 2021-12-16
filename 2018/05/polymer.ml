open! Core
open! Async
open! Import

let reactable_re =
  let lower = Char.all |> List.filter ~f:Char.is_lowercase in
  Re.compile
    (Re.alt
       (List.concat_map lower ~f:(fun c ->
          [ Re.seq [ Re.char c; Re.char (Char.uppercase c) ]
          ; Re.seq [ Re.char (Char.uppercase c); Re.char c ]
          ])))
;;

let rec react polymer =
  let polymer' = Re.replace_string reactable_re polymer ~by:"" in
  if String.length polymer' < String.length polymer then react polymer' else polymer
;;
