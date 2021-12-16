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

let rec value t =
  match t.children with
  | [] -> List.sum (module Int) t.metadata ~f:Fn.id
  | children ->
    let len = List.length children in
    let indices =
      List.filter_map t.metadata ~f:(fun i ->
        if i >= 1 && i <= len then Some (i - 1) else None)
    in
    List.foldi children ~init:0 ~f:(fun i acc c ->
      let count = List.count indices ~f:(( = ) i) in
      acc + if count = 0 then 0 else count * value c)
;;
