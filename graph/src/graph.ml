open! Core

type 'node t =
  { hashable : 'node Hashtbl.Hashable.t
  ; outgoing_edges : 'node -> 'node list
  ; incoming_edges : 'node -> 'node list
  ; nodes : 'node Hash_set.t option
  }
[@@deriving fields]

let of_functions ?(hashable = Hashtbl.Poly.hashable) ~incoming_edges ~outgoing_edges () =
  { hashable; outgoing_edges; incoming_edges; nodes = None }
;;

let of_edges ?(hashable = Hashtbl.Poly.hashable) edges ~edge_kind =
  let nodes = Hash_set.Using_hashable.create ~hashable () in
  List.iter edges ~f:(fun (from, to_) ->
    Hash_set.add nodes from;
    Hash_set.add nodes to_);
  match edge_kind with
  | `Directed ->
    let outgoing_edges = Hashtbl.Using_hashable.create ~hashable () in
    let incoming_edges = Hashtbl.Using_hashable.create ~hashable () in
    List.iter edges ~f:(fun (from, to_) ->
      Hashtbl.add_multi outgoing_edges ~key:from ~data:to_;
      Hashtbl.add_multi incoming_edges ~key:to_ ~data:from);
    { hashable
    ; outgoing_edges = Hashtbl.find_multi outgoing_edges
    ; incoming_edges = Hashtbl.find_multi incoming_edges
    ; nodes = Some nodes
    }
  | `Undirected ->
    let edge_table = Hashtbl.Using_hashable.create ~hashable () in
    List.iter edges ~f:(fun (from, to_) ->
      Hashtbl.add_multi edge_table ~key:from ~data:to_;
      Hashtbl.add_multi edge_table ~key:to_ ~data:from);
    { hashable
    ; outgoing_edges = Hashtbl.find_multi edge_table
    ; incoming_edges = Hashtbl.find_multi edge_table
    ; nodes = Some nodes
    }
;;

let bfs { hashable; outgoing_edges; incoming_edges = _; nodes = _ } ~start =
  let q = Queue.of_list [ start ] in
  let distance = Hashtbl.Using_hashable.of_alist_exn ~hashable [ start, 0 ] in
  while not (Queue.is_empty q) do
    let x = Queue.dequeue_exn q in
    let d = Hashtbl.find_exn distance x in
    outgoing_edges x
    |> List.iter ~f:(fun y ->
      match Hashtbl.mem distance y with
      | true -> ()
      | false ->
        Hashtbl.add_exn distance ~key:y ~data:(d + 1);
        Queue.enqueue q y)
  done;
  stage (Hashtbl.find distance)
;;

let error_no_node_set = Error.of_string "Graph does not know its node set"

let node_list t =
  t.nodes |> Result.of_option ~error:error_no_node_set |> Or_error.map ~f:Hash_set.to_list
;;
