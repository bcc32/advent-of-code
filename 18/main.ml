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
  [@@deriving compare, hash, sexp_of]
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
  [@@deriving compare, fields, hash, sexp_of]
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
    Array.fold grid ~init:Char.Set.empty ~f:(fun accum row ->
      Array.fold row ~init:accum ~f:(fun accum x ->
        match x with
        | Key c -> Set.add accum c
        | _ -> accum))
  in
  let graph =
    Graph.of_functions
      (module State')
      ~incoming_edges:(fun _ -> failwith "unimplemented")
      ~outgoing_edges:(fun ({ tl; tr; bl; br; collected_keys } as state) ->
        [ tl, State'.Fields.tl
        ; tr, State'.Fields.tr
        ; bl, State'.Fields.bl
        ; br, State'.Fields.br
        ]
        |> List.concat_map ~f:(fun ((i, j), field) ->
          Robot.Dir.all
          |> List.map ~f:(fun d -> Robot.Point.add (i, j) d)
          |> List.filter_map ~f:(fun (i, j) ->
            match grid.(i).(j) with
            | exception _ -> None
            | Door c
              when Bitset.mem collected_keys (Char.to_int c - Char.to_int 'a')
              -> Some (Field.fset field state (i, j))
            | Wall | Door _ -> None
            | Empty | Entrance -> Some (Field.fset field state (i, j))
            | Key c ->
              Some
                { (Field.fset field state (i, j)) with
                  collected_keys =
                    Bitset.add collected_keys (Char.to_int c - Char.to_int 'a')
                })))
  in
  Graph.bfs graph ~start:start_state
  |> Hashtbl.filter_keys ~f:(fun { collected_keys; _ } ->
    Bitset.length collected_keys = Set.length all_keys)
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
