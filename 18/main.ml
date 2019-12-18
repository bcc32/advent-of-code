open! Core
open! Async
open! Import

module Square = struct
  type t =
    | Wall
    | Empty
    | Key of int
    | Door of int
    | Entrance
  [@@deriving equal]

  let of_char_exn = function
    | '#' -> Wall
    | '.' -> Empty
    | 'a' .. 'z' as lower -> Key (Char.to_int lower - Char.to_int 'a')
    | 'A' .. 'Z' as upper -> Door (Char.to_int upper - Char.to_int 'A')
    | '@' -> Entrance
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
    { i : int
    ; j : int
    ; collected_keys : Bitset.t
    }
  [@@deriving compare, equal, hash, sexp_of]
end

let a () =
  let%bind grid = input () in
  let i, j =
    Array.find_mapi_exn grid ~f:(fun i row ->
      Array.find_mapi row ~f:(fun j x ->
        if [%equal: Square.t] x Entrance then Some j else None)
      |> Option.map ~f:(fun j -> i, j))
  in
  let all_keys =
    Array.fold grid ~init:Bitset.empty ~f:(fun accum row ->
      Array.fold row ~init:accum ~f:(fun accum square ->
        match square with
        | Key c -> Bitset.add accum c
        | _ -> accum))
  in
  let graph =
    Graph.of_functions
      (module State)
      ~incoming_edges:(fun _ -> failwith "unimplemented")
      ~outgoing_edges:(fun { i; j; collected_keys } ->
        Robot.Dir.all
        |> List.map ~f:(fun d -> Robot.Point.add (i, j) d)
        |> List.filter_map ~f:(fun (i, j) ->
          match grid.(i).(j) with
          | exception _ -> None
          | Door c when Bitset.mem collected_keys c ->
            Some { State.i; j; collected_keys }
          | Wall | Door _ -> None
          | Empty | Entrance -> Some { i; j; collected_keys }
          | Key c -> Some { i; j; collected_keys = Bitset.add collected_keys c }))
  in
  Graph.bfs graph ~start:{ i; j; collected_keys = Bitset.empty }
  |> Hashtbl.filter_keys ~f:(fun { i = _; j = _; collected_keys } ->
    Bitset.equal collected_keys all_keys)
  |> Hashtbl.data
  |> List.min_elt ~compare:[%compare: int]
  |> uw
  |> printf "%d\n";
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
