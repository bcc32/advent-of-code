open! Core
open! Async
open! Import

module Dir = struct
  type t =
    | Right
    | Up
    | Left
    | Down

  let apply t (row, col) =
    match t with
    | Right -> row, col + 1
    | Up -> row - 1, col
    | Left -> row, col - 1
    | Down -> row + 1, col
  ;;

  let anticlockwise = function
    | Right -> Up
    | Up -> Left
    | Left -> Down
    | Down -> Right
  ;;

  let clockwise = function
    | Right -> Down
    | Up -> Right
    | Left -> Up
    | Down -> Left
  ;;
end

module Turn = struct
  type t =
    | Left
    | Straight
    | Right

  let succ = function
    | Left -> Straight
    | Straight -> Right
    | Right -> Left
  ;;

  let apply t dir =
    match t with
    | Left -> Dir.anticlockwise dir
    | Straight -> dir
    | Right -> Dir.clockwise dir
  ;;
end

module Cart = struct
  (** sort in move order *)
  type t =
    { mutable moves : int
    ; mutable row : int
    ; mutable col : int
    ; mutable dir : Dir.t [@compare.ignore]
    ; mutable turn : Turn.t [@compare.ignore]
    }
  [@@deriving compare]

  let create row col dir =
    let dir =
      match dir with
      | '>' -> Dir.Right
      | '^' -> Up
      | '<' -> Left
      | 'v' -> Down
      | _ -> assert false
    in
    { moves = 0; row; col; dir; turn = Turn.Left }
  ;;
end

module Cart_heap = Hash_heap.Make (Tuple.Hashable_t (Int) (Int))

type t =
  { carts : Cart.t Cart_heap.t
  ; tracks : Tracks.t
  }

exception Collision of int * int

let maybe_turn tracks (cart : Cart.t) =
  match Tracks.get_track_exn tracks (cart.row, cart.col) with
  | '-' | '|' -> ()
  | '+' ->
    cart.dir <- Turn.apply cart.turn cart.dir;
    cart.turn <- Turn.succ cart.turn
  | '\\' ->
    cart.dir
    <- (match cart.dir with
      | Up | Down -> Dir.anticlockwise cart.dir
      | Left | Right -> Dir.clockwise cart.dir)
  | '/' ->
    cart.dir
    <- (match cart.dir with
      | Up | Down -> Dir.clockwise cart.dir
      | Left | Right -> Dir.anticlockwise cart.dir)
  | _ -> assert false
;;

let tick t ~on_collision =
  let move_no = (Cart_heap.top_exn t.carts).moves in
  let rec loop () =
    if Cart_heap.length t.carts > 0 && (Cart_heap.top_exn t.carts).moves = move_no
    then (
      let cart = Cart_heap.pop_exn t.carts in
      let row, col = Dir.apply cart.dir (cart.row, cart.col) in
      if Cart_heap.mem t.carts (row, col)
      then (
        on_collision ~row ~col;
        ignore (Cart_heap.find_pop_exn t.carts (row, col)))
      else (
        cart.row <- row;
        cart.col <- col;
        maybe_turn t.tracks cart;
        cart.moves <- cart.moves + 1;
        Cart_heap.push_exn t.carts ~key:(row, col) ~data:cart);
      loop ())
  in
  loop ()
;;

let lone_ranger t =
  if Cart_heap.length t.carts = 1
  then (
    let cart = Cart_heap.top_exn t.carts in
    Some (cart.row, cart.col))
  else if Cart_heap.length t.carts = 0
  then raise_s [%message "no carts"]
  else None
;;

let read () =
  let%bind tracks = Tracks.read () in
  let%bind grid =
    Reader.file_lines "input" >>| Array.of_list_map ~f:(fun row -> String.to_array row)
  in
  let carts = Cart_heap.create Cart.compare in
  let occupied =
    Array.make_matrix false ~dimx:(Array.length grid) ~dimy:(Array.length grid.(0))
  in
  Array.iteri grid ~f:(fun i row ->
    Array.iteri row ~f:(fun j -> function
      | ('^' | 'v' | '<' | '>') as c ->
        Cart_heap.push_exn carts ~key:(i, j) ~data:(Cart.create i j c);
        occupied.(i).(j) <- true
      | _ -> ()));
  return { carts; tracks }
;;
