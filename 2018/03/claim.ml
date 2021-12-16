open! Core
open! Async
open! Import

type t =
  { id : int
  ; x : int
  ; y : int
  ; w : int
  ; h : int
  }

let claim_re =
  Re.(
    compile
      (seq
         [ char '#'
         ; group (rep1 digit)
         ; str " @ "
         ; group (rep1 digit)
         ; char ','
         ; group (rep1 digit)
         ; str ": "
         ; group (rep1 digit)
         ; char 'x'
         ; group (rep1 digit)
         ]))
;;

let of_string line =
  let groups = Re.exec claim_re line in
  let get n = Re.Group.get groups n |> Int.of_string in
  { id = get 1; x = get 2; y = get 3; w = get 4; h = get 5 }
;;

let iter t ~f =
  for x = t.x to t.x + t.w - 1 do
    for y = t.y to t.y + t.h - 1 do
      f x y
    done
  done
;;
