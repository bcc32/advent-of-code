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

let next list elt =
  match Doubly_linked.next list elt with
  | None -> Doubly_linked.first_elt list |> Option.value_exn
  | Some next -> next
;;

let move cups ~turns =
  let max_elt = Doubly_linked.max_elt cups ~compare:Int.compare |> Option.value_exn in
  let get_elt_of_label =
    let first_cup_elt = Doubly_linked.first_elt cups |> Option.value_exn in
    let array = Array.init max_elt ~f:(fun _ -> first_cup_elt) in
    Doubly_linked.iter_elt cups ~f:(fun elt ->
      array.(Doubly_linked.Elt.value elt - 1) <- elt);
    fun label -> array.(label - 1)
  in
  let current_cup_elt = ref (Doubly_linked.first_elt cups |> Option.value_exn) in
  for _ = 1 to turns do
    let c1 = next cups !current_cup_elt in
    let c2 = next cups c1 in
    let c3 = next cups c2 in
    let dest_cup =
      let rec loop candidate =
        let candidate = if candidate < 1 then max_elt else candidate in
        if candidate = Doubly_linked.Elt.value c1
           || candidate = Doubly_linked.Elt.value c2
           || candidate = Doubly_linked.Elt.value c3
        then loop (candidate - 1)
        else candidate
      in
      loop (Doubly_linked.Elt.value !current_cup_elt - 1)
    in
    let dest_cup_elt = get_elt_of_label dest_cup in
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c3;
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c2;
    Doubly_linked.move_after cups ~anchor:dest_cup_elt c1;
    current_cup_elt := next cups !current_cup_elt
  done;
  get_elt_of_label
;;

let a () =
  let%bind cups = Lazy_deferred.force_exn Input.t in
  let cups = Doubly_linked.of_list cups in
  let get_elt_of_label = move cups ~turns:100 in
  let rec loop elt =
    if Doubly_linked.Elt.value elt <> 1
    then (
      printf "%d" (Doubly_linked.Elt.value elt);
      loop (next cups elt))
  in
  loop (next cups (get_elt_of_label 1));
  print_newline ();
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 25468379 |}];
  return ()
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
  let get_elt_of_label = move cups ~turns:10_000_000 in
  let one = get_elt_of_label 1 in
  let next1 = next cups one in
  let next2 = next cups next1 in
  print_s [%sexp (Doubly_linked.Elt.value next1 * Doubly_linked.Elt.value next2 : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 474747880250 |}];
  return ()
;;
