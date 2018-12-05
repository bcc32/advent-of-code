open! Core
open! Async
open! Import

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let claim_re =
  Re.(
    compile
      (seq
         [ char '#'
         ; rep1 digit
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
  { x = get 1; y = get 2; w = get 3; h = get 4 }
;;

let iter t ~f =
  for x = t.x to t.x + t.w - 1 do
    for y = t.y to t.y + t.h - 1 do
      f x y
    done
  done
;;
