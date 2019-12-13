open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Horizontal_paddle
    | Ball

  let of_int_exn = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Horizontal_paddle
    | 4 -> Ball
    | n -> invalid_argf "Tile.of_int_exn: unrecognized tile: %d" n ()
  ;;
end

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, hash, sexp_of]
end

let simulate program =
  let tiles = Hashtbl.create (module Point) in
  match Program.run program with
  | { input; output; done_ } ->
    Pipe.close input;
    let%bind () =
      Deferred.repeat_until_finished () (fun () ->
        match%bind Pipe.read_exactly output ~num_values:3 with
        | `Eof -> return (`Finished ())
        | `Fewer _ -> failwith "fewer"
        | `Exactly q ->
          let x = Queue.get q 0 in
          let y = Queue.get q 1 in
          let tile_id = Queue.get q 2 in
          Hashtbl.set tiles ~key:{ x; y } ~data:(Tile.of_int_exn tile_id);
          return (`Repeat ()))
    in
    let%bind () = done_ in
    return tiles
;;

let a () =
  let%bind program = input () in
  let%bind tiles = simulate program in
  Hashtbl.count tiles ~f:(function
    | Block -> true
    | Empty | Wall | Horizontal_paddle | Ball -> false)
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 357 |}]
;;

let b () =
  let%bind _input = input () in
  printf "output\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| output |}]
;;
