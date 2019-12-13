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

let simulate program ~user_input ~on_game_changed ~on_waiting_for_input =
  let tiles = Hashtbl.create (module Point) in
  let score = ref 0 in
  let%bind () = on_game_changed ~tiles ~score:!score in
  match Program.run' program ~on_waiting_for_input with
  | { input; output; done_ } ->
    let%map () =
      Deferred.repeat_until_finished () (fun () ->
        match%bind Pipe.read_exactly output ~num_values:3 with
        | `Eof -> return (`Finished ())
        | `Fewer _ -> failwith "fewer"
        | `Exactly q ->
          let x = Queue.get q 0 in
          let y = Queue.get q 1 in
          let tile_id = Queue.get q 2 in
          let key = { Point.x; y } in
          let score_changed = ref false in
          if [%equal: Point.t] key Point.score_loc
          then (
            score := tile_id;
            score_changed := true)
          else Hashtbl.set tiles ~key ~data:(Tile.of_int_exn tile_id);
          let%bind () =
            if !score_changed (* reached the end of batch *)
            then on_game_changed ~tiles ~score:!score
            else return ()
          in
          return (`Repeat ()))
    and () =
      let%map () =
        Pipe.transfer user_input input ~f:(function
          | `Left -> -1
          | `Neutral -> 0
          | `Right -> 1)
      in
      Pipe.close input
    and () = done_ in
    tiles
;;

(* TODO: Use [Pipe.empty] upon upgrading Async. *)
let a () =
  let%bind program = input () in
  let%bind tiles =
    simulate
      program
      ~user_input:(Pipe.of_list [])
      ~on_game_changed:(fun ~tiles:_ ~score:_ -> return ())
      ~on_waiting_for_input:(fun () -> return ())
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
  Deferred.ignore_m
    (simulate program ~user_input ~on_game_changed:draw ~on_waiting_for_input:(fun () ->
       return ()))
;;

let game_command =
  Command.async
    ~summary:"Play the block arcade game"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind term = Notty_async.Term.create () in
       let user_input =
         Pipe.filter_map (Notty_async.Term.events term) ~f:(function
           | `Mouse _ | `Paste _ | `Resize _ -> None
           | `Key (`Arrow `Left, _) -> Some `Left
           | `Key (`Arrow `Down, _) -> Some `Neutral
           | `Key (`Arrow `Right, _) -> Some `Right
           | `Key _ -> None)
       in
       let draw ~tiles ~score =
         if Hashtbl.is_empty tiles
         then return ()
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
           let image =
             Notty.I.tabulate (maxx + 1) (maxy + 1) (fun x y ->
               let char =
                 Hashtbl.find tiles { x; y }
                 |> Option.value ~default:Tile.Empty
                 |> Tile.to_char
               in
               Notty.I.char Notty.A.empty char 1 1)
           in
           let image = Notty.I.(strf "Score: %d" score <-> image) in
           Notty_async.Term.image term image)
       in
       play ~user_input ~draw)
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

let solve () =
  let%bind program = input () in
  program.memory.(0) <- 2;
  let score = ref 0 in
  let last_tiles = ref (Hashtbl.create (module Point)) in
  let user_input_r, user_input_w = Pipe.create () in
  let%bind tiles =
    simulate
      program
      ~user_input:user_input_r
      ~on_game_changed:(fun ~tiles ~score:new_score ->
        score := new_score;
        last_tiles := tiles;
        if debug then draw_plain ~tiles ~score:!score;
        return ())
      ~on_waiting_for_input:(fun () ->
        let find_tile_col tile =
          with_return_option (fun { return } ->
            Hashtbl.iteri !last_tiles ~f:(fun ~key:{ x; y = _ } ~data:tile' ->
              if [%equal: Tile.t] tile tile' then return x))
        in
        let joystick_action =
          let ball_col = find_tile_col Ball in
          let paddle_col = find_tile_col Horizontal_paddle in
          match ball_col, paddle_col with
          | None, _ | _, None -> `Neutral
          | Some target, Some current ->
            (match Ordering.of_int (Int.compare target current) with
             | Less -> `Left
             | Equal -> `Neutral
             | Greater -> `Right)
        in
        Pipe.write user_input_w joystick_action)
  in
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
