open! Core
open! Async
open! Import

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> Array.of_list_map ~f:String.to_array
;;

let find_labels grid =
  let results = ref [] in
  (* horizontal *)
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 2 do
      if Char.is_alpha grid.(i).(j) && Char.is_alpha grid.(i).(j + 1)
      then
        if j > 0 && Char.( = ) grid.(i).(j - 1) '.'
        then
          results
          := (String.of_char_list [ grid.(i).(j); grid.(i).(j + 1) ], (i, j - 1))
             :: !results
        else if j + 2 < Array.length grid.(0) && Char.( = ) grid.(i).(j + 2) '.'
        then
          results
          := (String.of_char_list [ grid.(i).(j); grid.(i).(j + 1) ], (i, j + 2))
             :: !results
        else assert false
    done
  done;
  (* vertical *)
  for i = 0 to Array.length grid - 2 do
    for j = 0 to Array.length grid.(0) - 1 do
      if Char.is_alpha grid.(i).(j) && Char.is_alpha grid.(i + 1).(j)
      then
        if i > 0 && Char.( = ) grid.(i - 1).(j) '.'
        then
          results
          := (String.of_char_list [ grid.(i).(j); grid.(i + 1).(j) ], (i - 1, j))
             :: !results
        else if i + 2 < Array.length grid && Char.( = ) grid.(i + 2).(j) '.'
        then
          results
          := (String.of_char_list [ grid.(i).(j); grid.(i + 1).(j) ], (i + 2, j))
             :: !results
        else assert false
    done
  done;
  !results
;;

let a () =
  let%bind grid = input () in
  let labels = find_labels grid in
  let by_label = labels |> String.Table.of_alist_multi in
  let by_point =
    labels |> List.map ~f:Tuple2.swap |> Hashtbl.of_alist_exn (module Robot.Point)
  in
  let graph =
    Graph.of_functions
      (module Robot.Point)
      ~incoming_edges:(fun _ -> assert false)
      ~outgoing_edges:(fun (i, j) ->
        (Robot.Dir.all
         |> List.map ~f:(fun d -> Robot.Point.add (i, j) d)
         |> List.filter ~f:(fun (i, j) ->
           match grid.(i).(j) with
           | exception _ -> false
           | '.' -> true
           | _ -> false))
        @
        match Hashtbl.find by_point (i, j) with
        | None -> []
        | Some label ->
          (match
             Hashtbl.find_multi by_label label
             |> List.filter ~f:(Fn.non ([%equal: int * int] (i, j)))
           with
           | [] -> []
           | [ x ] -> [ x ]
           | _ :: _ :: _ -> assert false))
  in
  let distance = Graph.bfs graph ~start:(Hashtbl.find_exn by_label "AA" |> List.hd_exn) in
  Hashtbl.find_exn distance (Hashtbl.find_exn by_label "ZZ" |> List.hd_exn)
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 602 |}]
;;

module Inner_outer = struct
  type 'a t =
    { inner : 'a
    ; outer : 'a
    }
end

let distance_from_wall grid (i, j) =
  [ i; j; Array.length grid - i - 1; Array.length grid.(0) - j - 1 ]
  |> List.reduce_exn ~f:Int.min
;;

module Labeled_point = struct
  type t =
    { point : Robot.Point.t
    ; label : string
    ; which : [ `Inner | `Outer | `Only ]
    }
  [@@deriving compare, hash, sexp_of, equal]
end

let dijkstra
      (type node)
      (module Key : Hashtbl.Key_plain with type t = node)
      ~outgoing_edges
      ~start
      ~is_end
  =
  let module HH = Hash_heap.Make (Key) in
  let distance = Hashtbl.create (module Key) in
  Hashtbl.add_exn distance ~key:start ~data:0;
  let frontier = HH.create [%compare: int] in
  HH.push_exn frontier ~key:start ~data:0;
  with_return_option (fun { return } ->
    while HH.length frontier > 0 do
      let node, dist = HH.pop_with_key_exn frontier in
      if is_end node then return (node, dist);
      outgoing_edges node
      |> Array.iter ~f:(fun (node', weight) ->
        let new_dist = dist + weight in
        match Hashtbl.find distance node' with
        | None ->
          Hashtbl.add_exn distance ~key:node' ~data:new_dist;
          HH.push_exn frontier ~key:node' ~data:new_dist
        | Some old_dist when new_dist < old_dist ->
          Hashtbl.set distance ~key:node' ~data:new_dist;
          HH.replace frontier ~key:node' ~data:new_dist
        | Some _ -> ())
    done)
;;

module State = struct
  type t =
    { point : Labeled_point.t
    ; level : int
    }
  [@@deriving compare, hash, sexp_of]
end

let b () =
  let%bind grid = input () in
  let labels = find_labels grid in
  let by_label =
    labels
    |> String.Table.of_alist_multi
    |> Hashtbl.map ~f:(function
      | [ x ] -> `One x
      | [ x; y ] ->
        if distance_from_wall grid x < distance_from_wall grid y
        then `Two { Inner_outer.outer = x; inner = y }
        else `Two { Inner_outer.inner = x; outer = y }
      | _ -> assert false)
  in
  let by_point =
    labels |> List.map ~f:Tuple2.swap |> Hashtbl.of_alist_exn (module Robot.Point)
  in
  let all_labeled_points =
    Hashtbl.to_alist by_label
    |> List.concat_map ~f:(fun (label, points) ->
      match points with
      | `One point -> [ { Labeled_point.point; label; which = `Only } ]
      | `Two { inner; outer } ->
        [ { Labeled_point.point = inner; label; which = `Inner }
        ; { Labeled_point.point = outer; label; which = `Outer }
        ])
    |> Array.of_list
  in
  let all_labeled_points_index_by_lp =
    Hashtbl.to_alist by_label
    |> List.concat_map ~f:(fun (label, points) ->
      match points with
      | `One point -> [ { Labeled_point.point; label; which = `Only } ]
      | `Two { inner; outer } ->
        [ { Labeled_point.point = inner; label; which = `Inner }
        ; { Labeled_point.point = outer; label; which = `Outer }
        ])
    |> List.mapi ~f:(fun i p -> p, i)
    |> Hashtbl.of_alist_exn (module Labeled_point)
  in
  let label_to_label_distance =
    let graph =
      Graph.of_functions
        (module Robot.Point)
        ~incoming_edges:(fun _ -> assert false)
        ~outgoing_edges:(fun (i, j) ->
          Robot.Dir.all
          |> List.map ~f:(fun d -> Robot.Point.add (i, j) d)
          |> List.filter ~f:(fun (i, j) ->
            match grid.(i).(j) with
            | exception _ -> false
            | '.' -> true
            | _ -> false))
    in
    Array.map all_labeled_points ~f:(fun p1 ->
      let distance = Graph.bfs graph ~start:p1.point in
      Array.map all_labeled_points ~f:(fun p2 -> Hashtbl.find distance p2.point))
  in
  let outgoing_edges_by_point_offset_by_level =
    Memo.general
      ~hashable:(Hashtbl.Hashable.of_key (module Labeled_point))
      (fun point ->
         let labeled_point_index = Hashtbl.find_exn all_labeled_points_index_by_lp point in
         Array.append
           (match point.which with
            | `Inner -> [| { State.point; level = 1 }, 1 |]
            | `Outer -> [| { State.point; level = -1 }, 1 |]
            | `Only -> [||])
           (label_to_label_distance.(labeled_point_index)
            |> Array.filter_mapi ~f:(fun i distance ->
              let next_point = all_labeled_points.(i) in
              match distance with
              | None -> None
              | Some distance ->
                Some ({ State.point = next_point; level = 0 }, distance))))
  in
  match
    dijkstra
      (module State)
      ~start:
        { point =
            { point =
                (Hashtbl.find_exn by_label "AA"
                 |> function
                 | `One x -> x
                 | `Two _ -> assert false)
            ; label = "AA"
            ; which = `Only
            }
        ; level = 0
        }
      ~is_end:(fun (state : State.t) ->
        match Hashtbl.find by_point state.point.point with
        | Some "ZZ" -> true
        | _ -> false)
      ~outgoing_edges:(fun { State.point; level } ->
        outgoing_edges_by_point_offset_by_level point
        |> Array.map
             ~f:
               (Tuple2.map_fst ~f:(fun (state : State.t) ->
                  { state with level = state.level + level })))
  with
  | None -> failwith "no path"
  | Some (_end, distance) ->
    printf "%d\n" distance;
    return ()
;;

let _ = b

(* let%expect_test "b" =
 *   let%bind () = b () in
 *   [%expect.unreachable]
 * [@@expect.uncaught_exn {|
 *   (\* CR expect_test_collector: This test expectation appears to contain a backtrace.
 *      This is strongly discouraged as backtraces are fragile.
 *      Please change this test to not include a backtrace. *\)
 *
 *   (monitor.ml.Error "Assert_failure 20/main.ml:228:11"
 *     ("<backtrace elided in test>" "Caught by monitor block_on_async"))
 *   Raised at file "src/result.ml" (inlined), line 187, characters 17-26
 *   Called from file "src/thread_safe.ml", line 131, characters 29-63
 *   Called from file "collector/expect_test_collector.ml", line 253, characters 12-19 |}]
 * ;; *)
