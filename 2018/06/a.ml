open! Core
open! Async
open! Import

let re = Re.(compile (seq [ group (rep1 digit); str ", "; group (rep1 digit) ]))
let dist (x, y) (x', y') = Int.abs (x - x') + Int.abs (y - y')

let main () =
  let%bind points =
    Reader.with_file "input" ~f:(fun r ->
      r
      |> Reader.lines
      |> Pipe.map ~f:(fun line ->
        let group = Re.exec re line in
        let x = Re.Group.get group 1 |> Int.of_string in
        let y = Re.Group.get group 2 |> Int.of_string in
        x, y)
      |> Pipe.to_list)
  in
  let min_x, max_x, min_y, max_y =
    let x0, y0 = List.hd_exn points in
    List.fold points ~init:(x0, x0, y0, y0) ~f:(fun (min_x, max_x, min_y, max_y) (x, y) ->
      Int.min min_x x, Int.max max_x x, Int.min min_y y, Int.max max_y y)
  in
  (* index to area *)
  let territory = Int.Table.create () in
  let infinite = Int.Hash_set.create () in
  let find_closest p =
    let min_dist =
      List.map points ~f:(dist p) |> List.min_elt ~compare:Int.compare |> Option.value_exn
    in
    List.filter_mapi points ~f:(fun i p' -> Option.some_if (dist p p' = min_dist) i)
  in
  for x = min_x - 1 to max_x + 1 do
    for y = min_y - 1 to max_y + 1 do
      match find_closest (x, y) with
      | [ i ] ->
        Hashtbl.incr territory i;
        if x < min_x || x > max_x || y < min_y || y > max_y then Hash_set.add infinite i
      | [] -> assert false
      | _ -> ()
      (* tied *)
    done
  done;
  if false
  then
    Debug.eprint_s
      [%message
        ""
          (points : (int * int) list)
          (territory : (int, int) Hashtbl.t)
          (infinite : int Hash_set.t)];
  Hashtbl.filter_keys_inplace territory ~f:(Fn.non (Hash_set.mem infinite));
  Hashtbl.data territory
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 5532 |}];
  return ()
;;
