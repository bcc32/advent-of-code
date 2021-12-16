open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let paint program ~starting_color =
  let robot = Robot.create_with_dir ~initial_loc:(0, 0) ~initial_dir:N in
  let paint =
    Hashtbl.create
      (module struct
        type t = int * int [@@deriving compare, hash, sexp_of]
      end)
  in
  Hashtbl.set paint ~key:(Robot.loc robot) ~data:starting_color;
  match Program.Async.run program with
  | { input; output; done_ } ->
    Pipe.write_without_pushback input starting_color;
    let%bind () =
      Deferred.repeat_until_finished () (fun () ->
        match%bind Pipe.read_exactly output ~num_values:2 with
        | `Fewer _ -> failwith "eof"
        | `Eof -> return (`Finished ())
        | `Exactly elts ->
          let color = Queue.get elts 0 in
          let turn = Queue.get elts 1 in
          Hashtbl.set paint ~key:(Robot.loc robot) ~data:color;
          Robot.turn robot (if turn = 0 then `Left else `Right);
          Robot.step_forward robot;
          Pipe.write_without_pushback
            input
            (Hashtbl.find paint (Robot.loc robot) |> Option.value ~default:0);
          return (`Repeat ()))
    in
    let%bind () = done_ in
    return paint
;;

let a () =
  let%bind program = input () in
  let%bind paint = paint program ~starting_color:0 in
  printf "%d\n" (Hashtbl.length paint);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 1747 |}]
;;

let min xs = List.min_elt xs ~compare:[%compare: int] |> Option.value_exn
let max xs = List.max_elt xs ~compare:[%compare: int] |> Option.value_exn

let b () =
  let%bind program = input () in
  let%bind paint = paint program ~starting_color:1 in
  let points = Hashtbl.keys paint in
  let minx = points |> List.map ~f:fst |> min in
  let maxx = points |> List.map ~f:fst |> max in
  let miny = points |> List.map ~f:snd |> min in
  let maxy = points |> List.map ~f:snd |> max in
  for y = maxy downto miny do
    for x = minx to maxx do
      match Hashtbl.find paint (x, y) with
      | None | Some 0 -> print_char ' '
      | Some 1 -> print_char '#'
      | _ -> assert false
    done;
    print_newline ()
  done;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect
    {|
    ####  ##   ##  ###  #  # #  # #    ###
       # #  # #  # #  # #  # # #  #    #  #
      #  #    #    #  # #### ##   #    ###
     #   #    # ## ###  #  # # #  #    #  #
    #    #  # #  # # #  #  # # #  #    #  #
    ####  ##   ### #  # #  # #  # #### ### |}]
;;
