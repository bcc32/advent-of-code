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
  let robot_input_r, robot_input_w = Pipe.create () in
  let robot_output_r, robot_output_w = Pipe.create () in
  let robot_done = Program.run program ~input:robot_input_r ~output:robot_output_w in
  Pipe.write_without_pushback robot_input_w starting_color;
  let%bind () =
    Deferred.repeat_until_finished () (fun () ->
      match%bind Pipe.read_exactly robot_output_r ~num_values:2 with
      | `Fewer _ -> failwith "eof"
      | `Exactly elts ->
        let color = Queue.get elts 0 in
        let turn = Queue.get elts 1 in
        Hashtbl.set paint ~key:(Robot.loc robot) ~data:color;
        Robot.turn robot (if turn = 0 then `Left else `Right);
        Robot.step_forward robot;
        Pipe.write_without_pushback
          robot_input_w
          (Hashtbl.find paint (Robot.loc robot) |> Option.value ~default:0);
        return (`Repeat ())
      | `Eof -> return (`Finished ()))
  in
  let%bind () = robot_done in
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
