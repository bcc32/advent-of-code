open! Core
open! Async
open! Import

let debug = false

module Square = struct
  type t =
    | Wall
    | Empty
    | Entrance
    | Key of int
    | Door of int
  [@@deriving equal]

  let of_char_exn = function
    | '#' -> Wall
    | '.' -> Empty
    | '@' -> Entrance
    | 'a' .. 'z' as lower -> Key (Char.to_int lower - Char.to_int 'a')
    | 'A' .. 'Z' as upper -> Door (Char.to_int upper - Char.to_int 'A')
    | c -> invalid_argf "Square.of_char_exn: %c" c ()
  ;;
end

let parse_line line = line |> String.to_array |> Array.map ~f:Square.of_char_exn

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> Array.of_list_map ~f:parse_line
;;

module State = struct
  type t =
    { key_point_index : int
    ; collected_keys : Bitset.t
    }
  [@@deriving compare, equal, hash, sexp_of]
end

let filter_points grid ~f =
  Array.concat_mapi grid ~f:(fun i row ->
    Array.filter_mapi row ~f:(fun j x -> if f x then Some (i, j) else None))
;;

let bfs key ~outgoing_edges ~start =
  let q = Queue.of_list [ start ] in
  let distance = Hashtbl.of_alist_exn key [ start, (0, []) ] in
  while not (Queue.is_empty q) do
    let x = Queue.dequeue_exn q in
    let d, accum = Hashtbl.find_exn distance x in
    outgoing_edges x
    |> List.iter ~f:(fun (y, a) ->
      match Hashtbl.mem distance y with
      | true -> ()
      | false ->
        Hashtbl.add_exn distance ~key:y ~data:(d + 1, a :: accum);
        Queue.enqueue q y)
  done;
  distance
;;

let a () =
  let%bind grid = input () in
  let starting_point =
    let a = filter_points grid ~f:([%equal: Square.t] Entrance) in
    assert (Array.length a = 1);
    a.(0)
  in
  let all_keys =
    Array.fold grid ~init:Bitset.empty ~f:(fun accum row ->
      Array.fold row ~init:accum ~f:(fun accum square ->
        match square with
        | Key c -> Bitset.add accum c
        | _ -> accum))
  in
  (* Also includes the entrance. *)
  let key_points =
    filter_points grid ~f:(function
      | Entrance -> true
      | Key _ -> true
      | _ -> false)
  in
  let entrance_index =
    Array.find_mapi_exn key_points ~f:(fun x p ->
      Option.some_if ([%equal: int * int] p starting_point) x)
  in
  let bfs ~start =
    bfs
      (module Robot.Point)
      ~start
      ~outgoing_edges:(fun point ->
        Robot.Dir.all
        |> List.map ~f:(fun d -> Robot.Point.add point d)
        |> List.filter_map ~f:(fun (i, j) ->
          match grid.(i).(j) with
          | exception _ -> None
          | Wall -> None
          | Empty | Entrance | Key _ -> Some ((i, j), None)
          | Door c -> Some ((i, j), Some c)))
  in
  let distances =
    (* We could only do about half of these because distance is symmetric, but
       BFS finds one-to-all distances so it's not that much cheaper. *)
    Array.map key_points ~f:(fun p1 ->
      let from_p1 = bfs ~start:p1 in
      Array.map key_points ~f:(fun p2 -> Hashtbl.find_exn from_p1 p2))
  in
  let final_state, distance =
    Graph.dijkstra
      (module State)
      ~start:{ key_point_index = entrance_index; collected_keys = Bitset.empty }
      ~is_end:(fun state -> Bitset.equal state.collected_keys all_keys)
      ~outgoing_edges:(fun { key_point_index; collected_keys } ->
        List.init (Array.length key_points) ~f:(fun i ->
          let distance, keys_needed = distances.(key_point_index).(i) in
          let collected_keys =
            let i, j = key_points.(i) in
            match grid.(i).(j) with
            | Key c -> Bitset.add collected_keys c
            | _ -> collected_keys
          in
          { State.key_point_index = i; collected_keys }, distance, keys_needed)
        |> List.filter_map ~f:(fun (state, distance, keys_needed) ->
          let keys_needed = List.filter_opt keys_needed |> Bitset.of_list in
          if Bitset.is_subset collected_keys ~subset:keys_needed
          then Some (state, distance)
          else None))
    |> Option.value_exn
  in
  if debug then print_s [%message (final_state : State.t)];
  printf "%d\n" distance;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 4700 |}]
;;

let input () =
  let%map lines = Reader.file_lines "input2" in
  lines |> Array.of_list_map ~f:parse_line
;;

module State' = struct
  type t =
    { tl : int * int
    ; tr : int * int
    ; bl : int * int
    ; br : int * int
    ; collected_keys : Bitset.t
    }
  [@@deriving compare, equal, fields, hash, sexp_of]
end

let b () =
  let%bind grid = input () in
  let start_state =
    let robots =
      Array.concat_mapi grid ~f:(fun i row ->
        Array.filter_mapi row ~f:(fun j x ->
          if [%equal: Square.t] x Entrance then Some j else None)
        |> Array.map ~f:(fun j -> i, j))
    in
    Array.sort robots ~compare:[%compare: int * int];
    match robots with
    | [| tl; tr; bl; br |] -> { State'.tl; tr; bl; br; collected_keys = Bitset.empty }
    | _ -> assert false
  in
  let all_keys =
    Array.fold grid ~init:Bitset.empty ~f:(fun accum row ->
      Array.fold row ~init:accum ~f:(fun accum x ->
        match x with
        | Key c -> Bitset.add accum c
        | _ -> accum))
  in
  let graph_one_moves field initial_state =
    Graph.of_functions
      (module State')
      ~incoming_edges:(fun _ -> failwith "unimplemented")
      ~outgoing_edges:(fun state ->
        let i, j = Field.get field state in
        let do_the_thing () =
          Robot.Dir.all
          |> List.map ~f:(fun d -> Robot.Point.add (i, j) d)
          |> List.filter_map ~f:(fun (i, j) ->
            match grid.(i).(j) with
            | exception _ -> None
            | Door c when Bitset.mem state.collected_keys c ->
              Some (Field.fset field state (i, j))
            | Wall | Door _ -> None
            | Empty | Entrance -> Some (Field.fset field state (i, j))
            | Key c ->
              Some
                { (Field.fset field state (i, j)) with
                  collected_keys = Bitset.add state.collected_keys c
                })
        in
        match grid.(i).(j) with
        | Door _ | Wall | Empty | Entrance -> do_the_thing ()
        | Key _ when [%equal: State'.t] state initial_state -> do_the_thing ()
        | Key _ ->
          (* don't move once we've hit a key *)
          [])
  in
  let outgoing_edges_and_weights state =
    [ State'.Fields.tl; State'.Fields.tr; State'.Fields.bl; State'.Fields.br ]
    |> List.concat_map ~f:(fun field ->
      let g = graph_one_moves field state in
      let d = Graph.bfs g ~start:state in
      Hashtbl.filter_keys d ~f:(fun state' ->
        let i, j = Field.get field state' in
        match grid.(i).(j) with
        | Key _ -> true
        | _ -> false)
      |> Hashtbl.to_alist)
  in
  let distances =
    let module HH = Hash_heap.Make (State') in
    let d = Hashtbl.create (module State') in
    let q = HH.create [%compare: int] in
    HH.push_exn q ~key:start_state ~data:0;
    let v = Hash_set.create (module State') in
    Hashtbl.add_exn d ~key:start_state ~data:0;
    while HH.length q > 0 do
      let x, dx = HH.pop_with_key_exn q in
      Hash_set.add v x;
      outgoing_edges_and_weights x
      |> List.iter ~f:(fun (y, w) ->
        let dy = dx + w in
        let dy =
          match Hashtbl.find d y with
          | None -> dy
          | Some dy' -> Int.min dy dy'
        in
        Hashtbl.set d ~key:y ~data:dy;
        if not (Hash_set.mem v y)
        then (
          HH.remove q y;
          HH.push_exn q ~key:y ~data:dy))
    done;
    d
  in
  Hashtbl.filter_keys distances ~f:(fun state ->
    Bitset.equal state.collected_keys all_keys)
  |> Hashtbl.data
  |> List.min_elt ~compare:[%compare: int]
  |> uw
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 72 |}]
;;
