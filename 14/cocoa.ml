open! Core
open! Async
open! Import

type t =
  { mutable elf_one : int
  ; mutable elf_two : int
  ; scores : int Queue.t
  }

let create () = { elf_one = 0; elf_two = 1; scores = Queue.of_list [ 3; 7 ] }

let digits n =
  let ones = n % 10 in
  let tens = n / 10 % 10 in
  if tens > 0 then [ tens; ones ] else [ ones ]
;;

let make_more t =
  let sum = Queue.get t.scores t.elf_one + Queue.get t.scores t.elf_two in
  Queue.enqueue_all t.scores (digits sum);
  let move i = (i + Queue.get t.scores i + 1) % Queue.length t.scores in
  t.elf_one <- move t.elf_one;
  t.elf_two <- move t.elf_two
;;

let length t = Queue.length t.scores
let sub t ~pos ~len = Array.init len ~f:(fun i -> Queue.get t.scores (pos + i))
