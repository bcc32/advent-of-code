open! Core

type 'node t

val of_functions
  :  (module Hashtbl.Key_plain with type t = 'node)
  -> incoming_edges:('node -> 'node list)
  -> outgoing_edges:('node -> 'node list)
  -> 'node t

val of_edges
  :  (module Hashtbl.Key_plain with type t = 'node)
  -> ('node * 'node) list
  -> edge_kind:[ `Directed | `Undirected ]
  -> 'node t

(* TODO: Use capability phantom type for this. *)
val node_list : 'node t -> 'node list Or_error.t
val outgoing_edges : 'node t -> 'node -> 'node list
val incoming_edges : 'node t -> 'node -> 'node list

(* TODO: Add early termination condition. *)
val bfs : 'node t -> start:'node -> ('node, int) Hashtbl.t
