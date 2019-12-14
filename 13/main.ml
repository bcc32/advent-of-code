open! Core
open! Async
open! Import
open Intcode

let debug = false
let input () = Reader.file_contents "input" >>| Program.of_string

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

let simulate program ~user_input ~on_game_changed =
  let tiles = Hashtbl.create (module Point) in
  let score = ref 0 in
  let%bind () = on_game_changed ~tiles ~score:!score in
  let output = Program.run' program ~input:user_input in
  let%bind () =
    Deferred.repeat_until_finished () (fun () ->
      match%bind Pipe.read_exactly output ~num_values:3 with
      | `Eof -> return (`Finished ())
      | `Fewer _ -> failwith "fewer"
      | `Exactly q ->
        let x = Queue.get q 0 in
        let y = Queue.get q 1 in
        let tile_id = Queue.get q 2 in
        let key = { Point.x; y } in
        if [%equal: Point.t] key Point.score_loc
        then score := tile_id
        else Hashtbl.set tiles ~key ~data:(Tile.of_int_exn tile_id);
        let%bind () = on_game_changed ~tiles ~score:!score in
        return (`Repeat ()))
  in
  let%bind () = Pipe.closed output in
  return tiles
;;

(* TODO: Use [Pipe.empty] upon upgrading Async. *)
let a () =
  let%bind program = input () in
  let%bind tiles =
    simulate
      program
      ~user_input:(Mvar.create () |> Mvar.read_only)
      ~on_game_changed:(fun ~tiles:_ ~score:_ -> return ())
  in
  Hashtbl.count tiles ~f:([%equal: Tile.t] Block) |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 357 |}]
;;

let play ~user_input ~draw =
  let%bind program = input () in
  program.memory.(0) <- 2;
  Deferred.ignore_m (simulate program ~user_input ~on_game_changed:draw)
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
          Hashtbl.find tiles { x; y }
          |> Option.value ~default:Tile.Empty
          |> Tile.to_char
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
       let user_input = Mvar.create () in
       don't_wait_for
         (Pipe.iter (Notty_async.Term.events term) ~f:(function
            | `Mouse _ | `Paste _ | `Resize _ -> return ()
            | `Key (`Arrow `Left, _) -> Mvar.put user_input (-1)
            | `Key (`Arrow `Down, _) -> Mvar.put user_input 0
            | `Key (`Arrow `Right, _) -> Mvar.put user_input 1
            | `Key _ -> return ()));
       let image = Mvar.create () in
       Deferred.forever () (fun () ->
         let%bind image = Mvar.take image in
         Notty_async.Term.image term image);
       play ~user_input:(Mvar.read_only user_input) ~draw:(fun ~tiles ~score ->
         Mvar.set image (term_image ~tiles ~score);
         return ()))
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
  program.memory.(0) <- 2;
  let score = ref 0 in
  (* FIXME: Unused *)
  let last_tiles = ref (Hashtbl.create (module Point)) in
  let ai_input = Mvar.create () in
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
  let%bind tiles =
    simulate
      program
      ~user_input:(Mvar.read_only ai_input)
      ~on_game_changed:(fun ~tiles ~score:new_score ->
        (* KLUDGE: Clear the Mvar while we recalculate.  If the program just
           took an input, wait a bit so the animation isn't too fast. *)
        let%bind () =
          match Mvar.take_now ai_input, term with
          | None, Some _ -> Clock_ns.after (Time_ns.Span.of_ms 10.)
          | None, None | Some _, _ -> return ()
        in
        score := new_score;
        last_tiles := tiles;
        if debug then draw_plain ~tiles ~score:!score;
        if Option.is_some term then Mvar.set image (term_image ~tiles ~score:!score);
        let find_tile_col tile =
          with_return_option (fun { return } ->
            Hashtbl.iteri !last_tiles ~f:(fun ~key:{ x; y = _ } ~data:tile' ->
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
        Mvar.set
          ai_input
          (match joystick_action with
           | `Left -> -1
           | `Neutral -> 0
           | `Right -> 1);
        return ())
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
