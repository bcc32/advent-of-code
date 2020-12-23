open! Core
open! Async
open! Import

let use_example_input = false

module Input = struct
  open! Advent_of_code_input_helpers

  type t = int list [@@deriving sexp_of]

  let parse input : t =
    input
    |> String.strip
    |> String.to_list
    |> List.map ~f:(fun char -> String.of_char char |> Int.of_string)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () ->
      Reader.file_contents
        (if use_example_input then "input.example.txt" else "input.txt")
      >>| parse)
  ;;
end

let move cups =
  let new_cups = Array.copy cups in
  let current_cup_label = new_cups.(0) in
  let removed, new_cups =
    ( Array.sub new_cups ~pos:1 ~len:3
    , Array.append [| current_cup_label |] (Array.subo new_cups ~pos:4) )
  in
  let min_cup_label = Array.min_elt cups ~compare:Int.compare |> uw in
  let max_cup_label = Array.max_elt cups ~compare:Int.compare |> uw in
  let dest_cup_index =
    let rec loop candidate =
      let candidate = if candidate < min_cup_label then max_cup_label else candidate in
      match Array.findi new_cups ~f:(fun _ -> ( = ) candidate) with
      | None -> loop (candidate - 1)
      | Some (i, _) -> i
    in
    loop (current_cup_label - 1)
  in
  let new_cups =
    Array.concat
      [ Array.sub new_cups ~pos:0 ~len:(dest_cup_index + 1)
      ; removed
      ; Array.subo new_cups ~pos:(dest_cup_index + 1)
      ]
  in
  let new_cups = Array.concat [ Array.subo new_cups ~pos:1; [| new_cups.(0) |] ] in
  Array.blit ~src:new_cups ~dst:cups ~src_pos:0 ~dst_pos:0 ~len:(Array.length cups)
;;

(* I mutated the input... *)

let a () =
  let%bind cups = Lazy_deferred.force_exn Input.t in
  let cups = Array.of_list cups in
  (* Current cup is always at front *)
  for _ = 1 to 100 do
    move cups
  done;
  let ans =
    let one_index, _ = Array.findi_exn cups ~f:(fun _ x -> x = 1) in
    String.concat_array
      (Array.concat
         [ Array.subo cups ~pos:(one_index + 1); Array.subo cups ~len:one_index ]
       |> Array.map ~f:Int.to_string)
  in
  print_endline ans;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 25468379 |}] in
  return ()
;;

let next list elt =
  match Doubly_linked.next list elt with
  | None -> Doubly_linked.first_elt list |> uw
  | Some next -> next
;;

let b () =
  let%bind cups = Lazy_deferred.force_exn Input.t >>| Array.of_list in
  (* init calls f with increasing indices *)
  let cups =
    let max_used = ref 0 in
    Array.init 1_000_000 ~f:(fun i ->
      let use = if i < Array.length cups then cups.(i) else !max_used + 1 in
      max_used := Int.max !max_used use;
      use)
  in
  let cups = Doubly_linked.of_array cups in
  let elt_of_label = Int.Table.create () in
  Doubly_linked.iter_elt cups ~f:(fun elt ->
    Hashtbl.add_exn elt_of_label ~key:(Doubly_linked.Elt.value elt) ~data:elt);
  let current_cup = ref (Doubly_linked.first cups |> uw) in
  for _ = 1 to 10_000_000 do
    let elt_of_current_cup = Hashtbl.find_exn elt_of_label !current_cup in
    let c1 = next cups elt_of_current_cup in
    let c2 = next cups c1 in
    let c3 = next cups c2 in
    let dest_cup =
      let rec loop candidate =
        let candidate = if candidate < 1 then 1_000_000 else candidate in
        if candidate = Doubly_linked.Elt.value c1
        || candidate = Doubly_linked.Elt.value c2
        || candidate = Doubly_linked.Elt.value c3
        then loop (candidate - 1)
        else candidate
      in
      loop (!current_cup - 1)
    in
    let dest_cup_elt = Hashtbl.find_exn elt_of_label dest_cup in
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c3;
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c2;
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c1;
    current_cup
    := Hashtbl.find_exn elt_of_label !current_cup
       |> next cups
       |> Doubly_linked.Elt.value
  done;
  let one = Hashtbl.find_exn elt_of_label 1 in
  let next1 = next cups one in
  let next2 = next cups next1 in
  Debug.eprint_s
    [%message (next1 : int Doubly_linked.Elt.t) (next2 : int Doubly_linked.Elt.t)];
  print_s [%sexp (Doubly_linked.Elt.value next1 * Doubly_linked.Elt.value next2 : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {|
    ((next1 723850) (next2 655865))
    474747880250 |}] in
  return ()
;;
