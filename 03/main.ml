open! Core
open! Async
open! Import

let follow path =
  let points = Hashtbl.create (module Robot.Point) in
  let robot = Robot.create_without_dir ~initial_loc:(0, 0) in
  let c = ref 0 in
  path
  |> List.iter ~f:(fun (n, dir) ->
    for _ = 1 to n do
      incr c;
      Robot.step_dir robot ~dir;
      ignore
        (Hashtbl.add points ~key:(Robot.loc robot) ~data:!c : [ `Duplicate | `Ok ])
    done);
  points
;;

let parse_path_part part =
  let dir = Robot.Dir.of_char_urdl_exn part.[0] in
  let n = Int.of_string (String.sub part ~pos:1 ~len:(String.length part - 1)) in
  n, dir
;;

let parse_path path = path |> String.split ~on:',' |> List.map ~f:parse_path_part

let points =
  Lazy_deferred.create (fun () ->
    let%bind lines = Reader.file_lines "input" in
    let first = List.nth_exn lines 0 |> parse_path in
    let second = List.nth_exn lines 1 |> parse_path in
    return (follow first, follow second))
;;

let a () =
  let%bind first, second = Lazy_deferred.force_exn points in
  Hashtbl.merge first second ~f:(fun ~key:point ->
    function
    | `Both _ -> Some point
    | `Left _ | `Right _ -> None)
  |> Hashtbl.keys
  |> List.map ~f:(fun p -> Robot.Point.dist_manhattan p (0, 0))
  |> List.min_elt ~compare:[%compare: int]
  |> Option.value_exn
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2050 |}]
;;

let b () =
  let%bind first, second = Lazy_deferred.force_exn points in
  let n =
    Hashtbl.merge first second ~f:(fun ~key:_ ->
      function
      | `Both (c1, c2) -> Some (c1 + c2)
      | `Left _ | `Right _ -> None)
    |> Hashtbl.data
    |> List.min_elt ~compare:[%compare: int]
    |> Option.value_exn
  in
  printf "%d\n" n;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 21666 |}]
;;
