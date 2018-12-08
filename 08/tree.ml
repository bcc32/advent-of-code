open! Core
open! Async
open! Import

type t =
  { children : t list
  ; metadata : int list
  }

let parse nums =
  let q = Queue.of_list nums in
  let rec parse () =
    let num_children = Queue.dequeue_exn q in
    let num_metadata = Queue.dequeue_exn q in
    let children = ref [] in
    for _ = 1 to num_children do
      children := parse () :: !children
    done;
    let children = List.rev !children in
    let metadata = ref [] in
    for _ = 1 to num_metadata do
      metadata := Queue.dequeue_exn q :: !metadata
    done;
    let metadata = List.rev !metadata in
    { children; metadata }
  in
  parse ()
;;

let rec sum_metadata t =
  List.sum (module Int) t.metadata ~f:Fn.id
  + List.sum (module Int) t.children ~f:sum_metadata
;;
