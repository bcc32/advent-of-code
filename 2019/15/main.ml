open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "aoc.in" >>| Program.of_string

let int_of_dir : Robot.Dir.t -> int = function
  | N -> 1
  | S -> 2
  | W -> 3
  | E -> 4
;;

module Material = struct
  type t =
    | Wall
    | Empty
    | Oxygen
  [@@deriving equal, sexp_of]

  let of_int_exn = function
    | 0 -> Wall
    | 1 -> Empty
    | 2 -> Oxygen
    | n -> invalid_argf "Material.of_int_exn: %d" n ()
  ;;
end

(* TODO: Add a way to represent programs persistently and immutably.  Use
   copy-on-write program states to efficiently explore the space without
   backtracking all the way to the origin each time. *)

let explore ~program =
  let robot = Robot.create_without_dir ~initial_loc:(0, 0) in
  let grid = Hashtbl.create (module Robot.Point) in
  let step dir =
    Program.Sync.provide_input program (int_of_dir dir);
    match Program.Sync.step program with
    | Done | Need_input -> failwith "expected output"
    | Output result ->
      let mat = Material.of_int_exn result in
      Hashtbl.set grid ~key:(Robot.Point.add (Robot.loc robot) dir) ~data:mat;
      (match mat with
       | Wall -> `Hit_wall
       | Empty | Oxygen ->
         Robot.step_dir robot ~dir;
         `Ok)
  in
  let step_exn dir =
    match step dir with
    | `Hit_wall -> failwith "hit a wall"
    | `Ok -> ()
  in
  let follow_exn path = List.iter path ~f:step_exn in
  let unfollow_exn path = follow_exn (List.rev_map path ~f:Robot.Dir.opp) in
  let bfs_queue = Queue.of_list [ 0, 0 ] in
  let path = Hashtbl.of_alist_exn (module Robot.Point) [ (0, 0), [] ] in
  let rec loop () =
    if Queue.is_empty bfs_queue
    then ()
    else (
      let x = Queue.dequeue_exn bfs_queue in
      let p = Hashtbl.find_exn path x in
      follow_exn (List.rev p);
      Robot.Dir.all
      |> List.map ~f:(fun d -> d, Robot.Point.add x d)
      |> List.iter ~f:(fun (dir, y) ->
        if Hashtbl.mem path y
        then ()
        else (
          match step dir with
          | `Hit_wall -> ()
          | `Ok ->
            Hashtbl.add_exn path ~key:y ~data:(dir :: p);
            Queue.enqueue bfs_queue y;
            step_exn (Robot.Dir.opp dir)));
      unfollow_exn (List.rev p);
      loop ())
  in
  loop ();
  grid, path
;;

let map =
  unstage
    (Deferred.Memo.unit (fun () ->
       let%map program = input () in
       explore ~program))
;;

let find_oxy_exn grid =
  Hashtbl.to_alist grid
  |> List.find_exn ~f:(fun (_point, material) -> [%equal: Material.t] material Oxygen)
  |> fst
;;

let a () =
  let%bind grid, path = map () in
  let oxy_point = find_oxy_exn grid in
  let path = Hashtbl.find_exn path oxy_point in
  printf "%d\n" (List.length path);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 298 |}];
  return ()
;;

let spread_oxygen grid start =
  let graph =
    Graph.of_functions
      (module Robot.Point)
      ~incoming_edges:(fun _ -> failwith "unimplemented")
      ~outgoing_edges:(fun point ->
        Robot.Point.adjacent point
        |> List.filter ~f:(fun p ->
          match (Hashtbl.find grid p : Material.t option) with
          | Some Wall -> false
          | None | Some Empty | Some Oxygen -> true))
  in
  let distance = Graph.bfs graph ~start in
  Hashtbl.data distance |> List.max_elt ~compare:[%compare: int] |> Option.value_exn
;;

let b () =
  let%bind grid, _path = map () in
  let oxy_point = find_oxy_exn grid in
  let spread_time = spread_oxygen grid oxy_point in
  printf "%d\n" spread_time;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 346 |}];
  return ()
;;
