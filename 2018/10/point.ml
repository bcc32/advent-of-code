open! Core
open! Async
open! Import

type t =
  { x : int
  ; y : int
  ; vx : int
  ; vy : int
  }

let re = Re.(compile (seq [ opt (char '-'); rep1 digit ]))

let of_string s =
  match Re.all re s |> List.map ~f:(fun g -> Re.Group.get g 0 |> Int.of_string) with
  | [ x; y; vx; vy ] -> { x; y; vx; vy }
  | _ -> raise_s [%message "wrong format" (s : string)]
;;

let step t = { t with x = t.x + t.vx; y = t.y + t.vy }
