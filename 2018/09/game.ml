open! Core
open! Async
open! Import

let re =
  Re.(
    compile
      (seq
         [ group (rep1 digit)
         ; str " players; last marble is worth "
         ; group (rep1 digit)
         ; str " points"
         ]))
;;

type t =
  { players : int
  ; last_marble : int
  }

let get () =
  let%bind str = Reader.file_contents "aoc.in" in
  let g = Re.exec re str in
  let players = Re.Group.get g 1 |> Int.of_string in
  let last_marble = Re.Group.get g 2 |> Int.of_string in
  return { players; last_marble }
;;

let simulate { players; last_marble } =
  let scores = Array.create 0 ~len:players in
  let current_marble = ref (Circle.create 0) in
  let current_player = ref 0 in
  let add_score n = scores.(!current_player) <- scores.(!current_player) + n in
  for marble = 1 to last_marble do
    if marble % 23 <> 0
    then current_marble := Circle.insert_after !current_marble.next marble
    else (
      add_score marble;
      let seven_left = !current_marble.prev.prev.prev.prev.prev.prev.prev in
      add_score seven_left.value;
      current_marble := Circle.remove_and_get_next seven_left);
    current_player := (!current_player + 1) % players
  done;
  scores
;;
