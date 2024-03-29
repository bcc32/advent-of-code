open! Core
open! Async
open! Import
open Intcode
open Program.Infix

let debug = false
let input () = Reader.file_contents "aoc.in" >>| Program.of_string

module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Horizontal_paddle
    | Ball
  [@@deriving equal]

  let of_int_exn = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Horizontal_paddle
    | 4 -> Ball
    | n -> invalid_argf "Tile.of_int_exn: unrecognized tile: %d" n ()
  ;;

  let to_char = function
    | Empty -> ' '
    | Wall -> '|'
    | Block -> '#'
    | Horizontal_paddle -> '_'
    | Ball -> '*'
  ;;
end

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, equal, fields, hash, sexp_of]

  let score_loc = { x = -1; y = 0 }
end

(* Input is async but output is completely synchronous. *)
let simulate program ~user_input ~on_game_changed ~tiles ~score =
  score := 0;
  on_game_changed ();
  let output = Queue.create () in
  let rec loop () =
    match Program.Sync.step program with
    | Done -> return ()
    | Need_input ->
      let%bind input = user_input () in
      Program.Sync.provide_input program input;
      loop ()
    | Output x ->
      Queue.enqueue output x;
      while Queue.length output >= 3 do
        let x = Queue.dequeue_exn output in
        let y = Queue.dequeue_exn output in
        let tile_id = Queue.dequeue_exn output in
        let key = { Point.x; y } in
        if [%equal: Point.t] key Point.score_loc
        then score := tile_id
        else Hashtbl.set tiles ~key ~data:(Tile.of_int_exn tile_id);
        on_game_changed ()
      done;
      loop ()
  in
  loop ()
;;

let a () =
  let%bind program = input () in
  let tiles = Hashtbl.create (module Point) in
  let score = ref 0 in
  let%bind () =
    simulate
      program
      ~user_input:(fun () -> assert false)
      ~on_game_changed:ignore
      ~tiles
      ~score
  in
  Hashtbl.count tiles ~f:([%equal: Tile.t] Block) |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 357 |}]
;;

let play ~user_input ~draw ~tiles ~score =
  let%bind program = input () in
  program.$(0) <- 2;
  simulate program ~user_input ~on_game_changed:draw ~tiles ~score
;;

let term_image ~tiles ~score =
  let image =
    if Hashtbl.is_empty tiles
    then Notty.I.empty
    else (
      let maxx =
        Hashtbl.keys tiles
        |> List.map ~f:Point.x
        |> List.max_elt ~compare:[%compare: int]
        |> uw
      in
      let maxy =
        Hashtbl.keys tiles
        |> List.map ~f:Point.y
        |> List.max_elt ~compare:[%compare: int]
        |> uw
      in
      Notty.I.tabulate (maxx + 1) (maxy + 1) (fun x y ->
        let char =
          Hashtbl.find tiles { x; y } |> Option.value ~default:Tile.Empty |> Tile.to_char
        in
        Notty.I.char Notty.A.empty char 1 1))
  in
  Notty.I.(strf "Score: %d" score <-> image)
;;

let play_command =
  Command.async
    ~summary:"Play the block arcade game"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind term = Notty_async.Term.create () ~nosig:false in
       let user_input =
         Pipe.filter_map (Notty_async.Term.events term) ~f:(function
           | `Mouse _ | `Paste _ | `Resize _ -> None
           | `Key (`Arrow `Left, _) -> Some (-1)
           | `Key (`Arrow `Down, _) -> Some 0
           | `Key (`Arrow `Right, _) -> Some 1
           | `Key _ -> None)
       in
       let image = Mvar.create () in
       Deferred.forever () (fun () ->
         let%bind image = Mvar.take image in
         Notty_async.Term.image term image);
       let tiles = Hashtbl.create (module Point) in
       let score = ref 0 in
       play
         ~user_input:(fun () ->
           match%map Pipe.read user_input with
           | `Eof -> failwith "eof"
           | `Ok x -> x)
         ~draw:(fun () -> Mvar.set image (term_image ~tiles ~score:!score))
         ~tiles
         ~score)
;;

let draw_plain ~tiles ~score =
  if Hashtbl.is_empty tiles
  then ()
  else (
    printf "Score: %d\n" score;
    let maxx =
      Hashtbl.keys tiles
      |> List.map ~f:Point.x
      |> List.max_elt ~compare:[%compare: int]
      |> uw
    in
    let maxy =
      Hashtbl.keys tiles
      |> List.map ~f:Point.y
      |> List.max_elt ~compare:[%compare: int]
      |> uw
    in
    for y = 0 to maxy do
      for x = 0 to maxx do
        Hashtbl.find tiles { x; y }
        |> Option.value ~default:Tile.Empty
        |> Tile.to_char
        |> print_char
      done;
      print_newline ()
    done)
;;

let solve ?term () =
  let%bind program = input () in
  program.$(0) <- 2;
  let score = ref 0 in
  let image = Mvar.create () in
  let finished = Ivar.create () in
  (match term with
   | None -> ()
   | Some term ->
     don't_wait_for
       (Deferred.repeat_until_finished () (fun () ->
          match%bind
            choose
              [ choice (Ivar.read finished) (fun () -> `Finished)
              ; choice (Mvar.take image) (fun image -> `Image image)
              ]
          with
          | `Finished -> return (`Finished ())
          | `Image image ->
            let%bind () = Notty_async.Term.image term image in
            return (`Repeat ()))));
  let tiles = Hashtbl.create (module Point) in
  let get_ai_input () =
    let find_tile_col tile =
      with_return_option (fun { return } ->
        Hashtbl.iteri tiles ~f:(fun ~key:{ Point.x; y = _ } ~data:tile' ->
          if [%equal: Tile.t] tile tile' then return x))
    in
    let joystick_action =
      match find_tile_col Ball, find_tile_col Horizontal_paddle with
      | None, _ | _, None -> `Neutral
      | Some target, Some current ->
        (match Ordering.of_int (Int.compare target current) with
         | Less -> `Left
         | Equal -> `Neutral
         | Greater -> `Right)
    in
    match joystick_action with
    | `Left -> -1
    | `Neutral -> 0
    | `Right -> 1
  in
  let%bind () =
    simulate
      program
      ~user_input:(fun () ->
        (* This map is necessary, even in the case where we immediately return. *)
        let open Eager_deferred.Let_syntax in
        let%map () =
          match term with
          | None -> return ()
          | Some _ -> Clock_ns.after (Time_ns.Span.of_ms 10.)
        in
        get_ai_input ())
      ~on_game_changed:(fun () ->
        if debug then draw_plain ~tiles ~score:!score;
        if Option.is_some term then Mvar.set image (term_image ~tiles ~score:!score))
      ~tiles
      ~score
  in
  Ivar.fill finished ();
  assert (not (Hashtbl.exists tiles ~f:([%equal: Tile.t] Block)));
  return !score
;;

let b () =
  let%bind score = solve () in
  printf "%d\n" score;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 17468 |}]
;;

let watch_command =
  Command.async
    ~summary:"Watch the AI play the block arcade game"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind term = Notty_async.Term.create () ~nosig:false in
       let%bind score = solve () ~term in
       let%bind () = Notty_async.Term.release term in
       printf "Final score: %d\n" score;
       return ())
;;

let game_command =
  Command.group
    ~summary:"Block arcade game"
    [ "play", play_command; "watch", watch_command ]
;;
