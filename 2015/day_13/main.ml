open! Core
open! Async
open! Import

(* FIXME: copy *)
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

let parse_line =
  let parser =
    let open Angstrom in
    let name = take_while1 Char.is_alpha in
    let* name1 = name in
    let* _ = string " would " in
    let* sign = string "gain" *> return 1 <|> string "lose" *> return (-1) in
    let* _ = string " " in
    let* amount = take_while1 Char.is_digit >>| Int.of_string in
    let* _ = string " happiness units by sitting next to " in
    let* name2 = name in
    let* _ = string "." in
    return (name1, sign * amount, name2)
  in
  fun s ->
    (* TODO: Add utility function for reporting parse errors *)
    Angstrom.parse_string parser s ~consume:All
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
;;

let gain_one table a b =
  List.find_map_exn table ~f:(fun (a', c, b') ->
    if String.equal a a' && String.equal b b' then Some c else None)
;;

let count_gain permutation table ~self =
  let gain = ref 0 in
  for
    i = 0 to if self then Array.length permutation - 2 else Array.length permutation - 1
  do
    let prev = permutation.(i) in
    let next = permutation.((i + 1) % Array.length permutation) in
    gain := !gain + gain_one table prev next + gain_one table next prev
  done;
  !gain
;;

let find_highest_gain table ~self =
  let set =
    table
    |> List.map ~f:fst3
    |> List.dedup_and_sort ~compare:String.compare
    |> Array.of_list
  in
  let max_gain = ref Int.min_value in
  let rec loop () =
    max_gain := Int.max !max_gain (count_gain set table ~self);
    if next_permutation set ~compare:String.compare then loop ()
  in
  loop ();
  !max_gain
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| (String.split_lines >> List.map ~f:parse_line))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let gain = find_highest_gain input ~self:false in
  print_s [%sexp (gain : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 709 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let gain = find_highest_gain input ~self:true in
  print_s [%sexp (gain : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 668 |}] in
  return ()
;;
