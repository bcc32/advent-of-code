open! Core

let tunnels = String.Table.create ()
let flows = String.Table.create ()
let node_indexes = String.Table.create ()

let () =
  let re =
    let open Re in
    compile
      (seq
         [ str "Valve "
         ; group (seq [ any; any ])
         ; str " has flow rate="
         ; group (rep1 digit)
         ; rep1 any
         ; alt [ str "valves "; str "valve " ]
         ; group (rep1 any)
         ])
  in
  let commas =
    let open Re in
    compile (str ", ")
  in
  In_channel.read_lines "aoc.in"
  |> List.iteri ~f:(fun i line ->
       let g = Re.exec re line in
       let from = Re.Group.get g 1 in
       let flow = Re.Group.get g 2 |> Int.of_string in
       let to_ = Re.Group.get g 3 |> Re.split commas in
       Hashtbl.add_exn tunnels ~key:from ~data:to_;
       Hashtbl.add_exn flows ~key:from ~data:flow;
       Hashtbl.add_exn node_indexes ~key:from ~data:i)
;;

let tunnels =
  tunnels
  |> Hashtbl.to_alist
  |> List.map ~f:(fun (k, d) ->
       Hashtbl.find_exn node_indexes k, List.map d ~f:(Hashtbl.find_exn node_indexes))
  |> Int.Table.of_alist_exn
;;

let flows =
  flows
  |> Hashtbl.to_alist
  |> List.map ~f:(Tuple2.map_fst ~f:(Hashtbl.find_exn node_indexes))
  |> Int.Table.of_alist_exn
;;

module State = struct
  type t =
    { node : int
    ; ele_node : int
    ; nodes_open : int
    }
  [@@deriving compare, equal, hash, sexp_of]

  let node_is_open t node = t.nodes_open land (1 lsl node) <> 0
  let open_node t node = { t with nodes_open = t.nodes_open lor (1 lsl node) }

  let neighbors t ~minutes_left_after_step =
    let after_me =
      let after_my_open =
        if (not (node_is_open t t.node)) && Hashtbl.find_exn flows t.node > 0
        then
          [ minutes_left_after_step * Hashtbl.find_exn flows t.node, open_node t t.node ]
        else []
      in
      let after_my_move =
        List.map (Hashtbl.find_exn tunnels t.node) ~f:(fun neighbor_node ->
          0, { t with node = neighbor_node })
      in
      after_my_open @ after_my_move
    in
    List.concat_map after_me ~f:(fun (score, t) ->
      let after_elephant =
        let after_elephant_open =
          if (not (node_is_open t t.ele_node)) && Hashtbl.find_exn flows t.ele_node > 0
          then
            [ ( minutes_left_after_step * Hashtbl.find_exn flows t.ele_node
              , open_node t t.ele_node )
            ]
          else []
        in
        let after_elephant_move =
          List.map (Hashtbl.find_exn tunnels t.ele_node) ~f:(fun neighbor_ele_node ->
            0, { t with ele_node = neighbor_ele_node })
        in
        after_elephant_open @ after_elephant_move
      in
      List.map after_elephant ~f:(fun (score', s) -> score + score', s))
  ;;
end

let dijkstra start =
  let rec loop minutes_left_after_step scores =
    if minutes_left_after_step < 0
    then scores
    else (
      Debug.eprint_s
        [%message (minutes_left_after_step : int) (Hashtbl.length scores : int)];
      let new_scores = Hashtbl.create (module State) in
      Hashtbl.iteri scores ~f:(fun ~key:state ~data:score ->
        State.neighbors state ~minutes_left_after_step
        |> List.iter ~f:(fun (added_score, neighbor) ->
             match Hashtbl.find new_scores neighbor with
             | None -> Hashtbl.set new_scores ~key:neighbor ~data:(added_score + score)
             | Some s when added_score + score > s ->
               Hashtbl.set new_scores ~key:neighbor ~data:(added_score + score)
             | _ -> ()));
      loop (minutes_left_after_step - 1) new_scores)
  in
  loop 25 (Hashtbl.of_alist_exn (module State) [ start, 0 ])
;;

let max_flows =
  let aa = Hashtbl.find_exn node_indexes "AA" in
  dijkstra ({ node = aa; ele_node = aa; nodes_open = 0 } : State.t)
;;

let () =
  let ans =
    max_flows |> Hashtbl.data |> List.max_elt ~compare:Int.compare |> Option.value_exn
  in
  print_s [%message (ans : int)]
;;

(* I was able to rewrite my solution in OCaml and optimize it before the Ruby
   solution finished.  It still took 20 minutes to run. *)
