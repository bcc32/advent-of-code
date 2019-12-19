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

(* TODO: Add a way to represent programs persistently and immutably. *)

let explore ~program =
  match Program.Async.run program with
  | { input; output; done_ = _ } ->
    let robot = Robot.create_without_dir ~initial_loc:(0, 0) in
    let grid = Hashtbl.create (module Robot.Point) in
    let step dir =
      let%bind () = Pipe.write input (int_of_dir dir) in
      match%bind Pipe.read output with
      | `Eof -> failwith "eof"
      | `Ok result ->
        let mat = Material.of_int_exn result in
        Hashtbl.set grid ~key:(Robot.Point.add (Robot.loc robot) dir) ~data:mat;
        (match mat with
         | Wall -> return `Hit_wall
         | Empty | Oxygen ->
           Robot.step_dir robot ~dir;
           return `Ok)
    in
    let step_exn dir =
      match%map step dir with
      | `Hit_wall -> failwith "hit a wall"
      | `Ok -> ()
    in
    let follow_exn path = Deferred.List.iter path ~f:step_exn in
    let unfollow_exn path = follow_exn (List.rev_map path ~f:Robot.Dir.opp) in
    let bfs_queue = Queue.of_list [ 0, 0 ] in
    let path = Hashtbl.of_alist_exn (module Robot.Point) [ (0, 0), [] ] in
    let rec loop () =
      if Queue.is_empty bfs_queue
      then return ()
      else (
        let x = Queue.dequeue_exn bfs_queue in
        let p = Hashtbl.find_exn path x in
        let%bind () = follow_exn (List.rev p) in
        let%bind () =
          Robot.Dir.all
          |> List.map ~f:(fun d -> d, Robot.Point.add x d)
          |> Deferred.List.iter ~f:(fun (dir, y) ->
            if Hashtbl.mem path y
            then return ()
            else (
              match%bind step dir with
              | `Hit_wall -> return ()
              | `Ok ->
                Hashtbl.add_exn path ~key:y ~data:(dir :: p);
                Queue.enqueue bfs_queue y;
                let%bind () = step_exn (Robot.Dir.opp dir) in
                return ()))
        in
        let%bind () = unfollow_exn (List.rev p) in
        loop ())
    in
    let%bind () = loop () in
    return (grid, path)
;;

let find_oxy_exn grid =
  Hashtbl.to_alist grid
  |> List.find_exn ~f:(fun (_point, material) -> [%equal: Material.t] material Oxygen)
  |> fst
;;

let a () =
  let%bind program = input () in
  let%bind grid, path = explore ~program in
  let oxy_point = find_oxy_exn grid in
  let path = Hashtbl.find_exn path oxy_point in
  printf "%d\n" (List.length path);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 298 |}]
;;

let spread_oxygen grid start =
  let graph =
    Graph.of_functions
      (module Robot.Point)
      ~incoming_edges:(fun _ -> failwith "unimplemented")
      ~outgoing_edges:(fun point ->
        Robot.Dir.all
        |> List.map ~f:(fun d -> Robot.Point.add point d)
        |> List.filter ~f:(fun p ->
          match (Hashtbl.find grid p : Material.t option) with
          | Some Wall -> false
          | None | Some Empty | Some Oxygen -> true))
  in
  let distance = Graph.bfs graph ~start in
  Hashtbl.data distance |> List.max_elt ~compare:[%compare: int] |> Option.value_exn
;;

let b () =
  let%bind program = input () in
  let%bind grid, _path = explore ~program in
  let oxy_point = find_oxy_exn grid in
  let spread_time = spread_oxygen grid oxy_point in
  printf "%d\n" spread_time;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 346 |}]
;;
