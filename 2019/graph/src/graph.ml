open! Core

type 'node t =
  { key : (module Hashtbl.Key_plain with type t = 'node)
  ; outgoing_edges : 'node -> 'node list
  ; incoming_edges : 'node -> 'node list
  ; nodes : 'node Hash_set.t option
  }
[@@deriving fields]

let of_functions key ~incoming_edges ~outgoing_edges =
  { key; outgoing_edges; incoming_edges; nodes = None }
;;

let of_edges key edges ~edge_kind =
  let nodes = Hash_set.create key in
  List.iter edges ~f:(fun (from, to_) ->
    Hash_set.add nodes from;
    Hash_set.add nodes to_);
  match edge_kind with
  | `Directed ->
    let outgoing_edges = Hashtbl.create key in
    let incoming_edges = Hashtbl.create key in
    List.iter edges ~f:(fun (from, to_) ->
      Hashtbl.add_multi outgoing_edges ~key:from ~data:to_;
      Hashtbl.add_multi incoming_edges ~key:to_ ~data:from);
    { key
    ; outgoing_edges = Hashtbl.find_multi outgoing_edges
    ; incoming_edges = Hashtbl.find_multi incoming_edges
    ; nodes = Some nodes
    }
  | `Undirected ->
    let edge_table = Hashtbl.create key in
    List.iter edges ~f:(fun (from, to_) ->
      Hashtbl.add_multi edge_table ~key:from ~data:to_;
      Hashtbl.add_multi edge_table ~key:to_ ~data:from);
    { key
    ; outgoing_edges = Hashtbl.find_multi edge_table
    ; incoming_edges = Hashtbl.find_multi edge_table
    ; nodes = Some nodes
    }
;;

let bfs { key; outgoing_edges; incoming_edges = _; nodes = _ } ~start =
  let q = Queue.of_list [ start ] in
  let distance = Hashtbl.of_alist_exn key [ start, 0 ] in
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
  distance
;;

let error_no_node_set = Error.of_string "Graph does not know its node set"

let node_list t =
  t.nodes |> Result.of_option ~error:error_no_node_set |> Or_error.map ~f:Hash_set.to_list
;;

let dijkstra
      (type node)
      (module Key : Hashtbl.Key_plain with type t = node)
      ~outgoing_edges
      ~start
      ~is_end
  =
  let module HH = Hash_heap.Make (Key) in
  let distance = Hashtbl.create (module Key) in
  Hashtbl.add_exn distance ~key:start ~data:0;
  let frontier = HH.create [%compare: int] in
  HH.push_exn frontier ~key:start ~data:0;
  with_return_option (fun { return } ->
    while HH.length frontier > 0 do
      let node, dist = HH.pop_with_key_exn frontier in
      if is_end node then return (node, dist);
      outgoing_edges node
      |> List.iter ~f:(fun (node', weight) ->
        let new_dist = dist + weight in
        match Hashtbl.find distance node' with
        | None ->
          Hashtbl.add_exn distance ~key:node' ~data:new_dist;
          HH.push_exn frontier ~key:node' ~data:new_dist
        | Some old_dist when new_dist < old_dist ->
          Hashtbl.set distance ~key:node' ~data:new_dist;
          HH.replace frontier ~key:node' ~data:new_dist
        | Some _ -> ())
    done)
;;
