open! Core
open! Async
open! Import

type 'a t =
  { value : 'a
  ; mutable prev : 'a t
  ; mutable next : 'a t
  }

let create value =
  let rec t = { value; prev = t; next = t } in
  t
;;

let insert_after t x =
  let node = { value = x; prev = t; next = t.next } in
  t.next.prev <- node;
  t.next <- node;
  node
;;

let remove_and_get_next t =
  let t' = t.prev in
  assert (not (phys_equal t t'));
  t'.next <- t.next;
  t.next.prev <- t';
  t.next
;;
