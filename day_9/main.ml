open! Core
open! Async
open! Import

let parse_line =
  let parser =
    let open Angstrom in
    let city = take_while1 Char.is_alpha in
    let dist =
      let+ digits = take_while1 Char.is_digit in
      Int.of_string digits
    in
    map3 city (string " to " *> city) (string " = " *> dist) ~f:(fun l r d -> l, r, d)
  in
  fun string ->
    Angstrom.parse_string parser string ~consume:All
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
;;

let make_data dists =
  let t = String.Table.create () in
  List.iter dists ~f:(fun (l, r, d) ->
    Hashtbl.add_multi t ~key:l ~data:(r, d);
    Hashtbl.add_multi t ~key:r ~data:(l, d));
  t
;;

let next_permutation array ~compare =
  let rec find_first_sorted_from_end pos =
    if pos < 0
    then None
    else if compare array.(pos) array.(pos + 1) < 0
    then Some pos
    else find_first_sorted_from_end (pos - 1)
  in
  match find_first_sorted_from_end (Array.length array - 2) with
  | None -> false
  | Some to_increase ->
    let rec find_next_largest pos current_min arg =
      if pos >= Array.length array
      then arg
      else if compare array.(pos) current_min < 0
           && compare array.(pos) array.(to_increase) > 0
      then find_next_largest (pos + 1) array.(pos) pos
      else find_next_largest (pos + 1) current_min arg
    in
    let swap_with =
      find_next_largest (to_increase + 1) array.(to_increase + 1) (to_increase + 1)
    in
    Array.swap array to_increase swap_with;
    Array.sort array ~pos:(to_increase + 1) ~compare;
    true
;;

let count_cost places data =
  let c = ref 0 in
  for i = 0 to Array.length places - 2 do
    let a = places.(i) in
    let b = places.(i + 1) in
    let dist_from_a = Hashtbl.find_exn data a in
    let dist = List.Assoc.find_exn dist_from_a b ~equal:String.equal in
    c := !c + dist
  done;
  !c
;;

let find_shortest t =
  let places = Hashtbl.keys t |> Array.of_list in
  Array.sort places ~compare:String.compare;
  let min_cost = ref Int.max_value in
  let rec loop () =
    let cost = count_cost places t in
    min_cost := Int.min cost !min_cost;
    if next_permutation places ~compare:String.compare then loop ()
  in
  loop ();
  !min_cost
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| (String.split_lines >> List.map ~f:parse_line >> make_data))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let cost = find_shortest input in
  print_s [%sexp (cost : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 141 |}] in
  return ()
;;

let find_longest t =
  let places = Hashtbl.keys t |> Array.of_list in
  Array.sort places ~compare:String.compare;
  let max_cost = ref Int.min_value in
  let rec loop () =
    let cost = count_cost places t in
    max_cost := Int.max cost !max_cost;
    if next_permutation places ~compare:String.compare then loop ()
  in
  loop ();
  !max_cost
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let cost = find_longest input in
  print_s [%sexp (cost : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 736 |}] in
  return ()
;;
