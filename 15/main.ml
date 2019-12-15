open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let int_of_dir : Robot.Dir.t -> int = function
  | N -> 1
  | S -> 2
  | W -> 3
  | E -> 4
;;

let what's_there ~program ~path =
  let program = Program.copy program in
  match Program.run program with
  | { input; output; done_ = _ } ->
    List.iter path ~f:(fun p -> Pipe.write_without_pushback input (int_of_dir p));
    (match%map Pipe.read_exactly output ~num_values:(List.length path) with
     | `Eof | `Fewer _ -> failwith "eof"
     | `Exactly q -> Queue.last_exn q)
;;

let search_grid ~program =
  let paths = Queue.of_list [ [ Robot.Dir.N ]; [ S ]; [ W ]; [ E ] ] in
  let rec loop () =
    if Queue.is_empty paths then failwith "no oxygen";
    let path = Queue.dequeue_exn paths in
    (* Debug.eprint_s [%sexp (path : Cmd.t list)]; *)
    match%bind what's_there ~program ~path:(List.rev path) with
    | 0 -> loop ()
    | 1 ->
      Robot.Dir.all
      |> List.filter ~f:(fun cmd ->
        not ([%equal: Robot.Dir.t] (Robot.Dir.opp (List.hd_exn path)) cmd))
      |> List.iter ~f:(fun step -> Queue.enqueue paths (step :: path));
      loop ()
    | 2 -> return (List.length path)
    | _ -> assert false
  in
  loop ()
;;

let a () =
  let%bind program = input () in
  let%bind soln = search_grid ~program in
  printf "%d\n" soln;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 298 |}]
;;

let end_location (path : Robot.Dir.t list) =
  List.fold path ~init:(0, 0) ~f:(fun (x, y) dir ->
    match dir with
    | N -> x, y + 1
    | S -> x, y - 1
    | E -> x + 1, y
    | W -> x - 1, y)
;;

let search_grid ~program =
  let paths = Queue.of_list [ [ Robot.Dir.N ]; [ S ]; [ W ]; [ E ] ] in
  let grid = Hashtbl.create (module Robot.Point) in
  let oxy_loc = ref (0, 0) in
  let rec loop () =
    if Queue.is_empty paths
    then return ()
    else (
      let path = Queue.dequeue_exn paths in
      (* Debug.eprint_s [%sexp (path : Cmd.t list)]; *)
      match%bind what's_there ~program ~path:(List.rev path) with
      | 0 ->
        Hashtbl.set grid ~key:(end_location path) ~data:0;
        loop ()
      | 1 ->
        Hashtbl.set grid ~key:(end_location path) ~data:1;
        Robot.Dir.all
        |> List.filter ~f:(fun cmd ->
          not ([%equal: Robot.Dir.t] (Robot.Dir.opp (List.hd_exn path)) cmd))
        |> List.iter ~f:(fun step -> Queue.enqueue paths (step :: path));
        loop ()
      | 2 ->
        Hashtbl.set grid ~key:(end_location path) ~data:2;
        oxy_loc := end_location path;
        loop ()
      | _ -> assert false)
  in
  let%bind () = loop () in
  return (grid, !oxy_loc)
;;

let spread_oxygen grid (x, y) =
  let q = Queue.of_list [ x, y ] in
  let dist = Hashtbl.create (module Robot.Point) in
  Hashtbl.set dist ~key:(x, y) ~data:0;
  while not (Queue.is_empty q) do
    let p = Queue.dequeue_exn q in
    let d = Hashtbl.find_exn dist p in
    Robot.Dir.all
    |> List.iter ~f:(fun dir ->
      let p' = Robot.Point.add p dir in
      if (not (Hashtbl.mem dist p'))
      && [%equal: int option] (Hashtbl.find grid p') (Some 1)
      then (
        Hashtbl.set dist ~key:p' ~data:(d + 1);
        Queue.enqueue q p'))
  done;
  Hashtbl.data dist |> List.max_elt ~compare:Int.compare |> uw
;;

let b () =
  let%bind program = input () in
  let%bind grid, oxy_loc = search_grid ~program in
  let spread_time = spread_oxygen grid oxy_loc in
  printf "%d\n" spread_time;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {|
    346 |}]
;;
