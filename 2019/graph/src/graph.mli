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

val node_list : 'node t -> 'node list Or_error.t
val outgoing_edges : 'node t -> 'node -> 'node list
val incoming_edges : 'node t -> 'node -> 'node list

val bfs
  :  ?finish_early_if_dequeued:('node -> bool)
  -> 'node t
  -> start:'node
  -> ('node, int) Hashtbl.t

val dijkstra
  :  (module Hashtbl.Key_plain with type t = 'node)
  -> outgoing_edges:('node -> ('node * int) list)
  -> start:'node
  -> is_end:('node -> bool)
  -> ('node * int) option
