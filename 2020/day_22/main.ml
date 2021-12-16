open! Core
open! Async
open! Import

module Deck = struct
  type t = int Fqueue.t [@@deriving compare, hash, sexp_of]

  let equal = [%compare.equal: t]
  let of_lines = List.map ~f:Int.of_string >> Fqueue.of_list
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t =
    { p1 : Deck.t
    ; p2 : Deck.t
    }
  [@@deriving compare, equal, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  let parse input : t =
    let ps = input |> lines |> paragraphs in
    let decks = ps |> List.map ~f:List.tl_exn in
    match decks |> List.map ~f:Deck.of_lines with
    | [ p1; p2 ] -> { p1; p2 }
    | _ -> assert false
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

exception Done

let play decks =
  let round ({ p1; p2 } : Input.t) =
    match Fqueue.dequeue p1, Fqueue.dequeue p2 with
    | Some (p1h, p1t), Some (p2h, p2t) ->
      let p1, p2 =
        match Ordering.of_int (Int.compare p1h p2h) with
        | Greater -> Fqueue.enqueue (Fqueue.enqueue p1t p1h) p2h, p2t
        | Less -> p1t, Fqueue.enqueue (Fqueue.enqueue p2t p2h) p1h
        | Equal -> failwith "ambiguous"
      in
      ({ p1; p2 } : Input.t)
    | None, _ | _, None -> raise Done
  in
  let rec loop state =
    match round state with
    | exception Done -> state
    | state -> loop state
  in
  loop decks
;;

let score deck =
  deck
  |> Fqueue.to_list
  |> List.fold_right ~init:(0, 1) ~f:(fun x (sum, multiplier) ->
    sum + (multiplier * x), multiplier + 1)
  |> fst
;;

let a () =
  let%bind decks = Lazy_deferred.force_exn Input.t in
  let decks = play decks in
  print_s [%sexp (Int.max (score decks.p1) (score decks.p2) : int)];
  (* print_s [%sexp (decks : Input.t)]; *)
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 32677 |}] in
  return ()
;;

exception Victory of [ `P1 | `P2 ]

let rec play_recursive (decks : Input.t) =
  let round ({ p1; p2 } : Input.t) =
    match Fqueue.dequeue p1, Fqueue.dequeue p2 with
    | Some (p1h, p1t), Some (p2h, p2t) ->
      let p1, p2 =
        if Fqueue.length p1t >= p1h && Fqueue.length p2t >= p2h
        then (
          match
            play_recursive
              { p1 = Fqueue.of_list (Fqueue.to_list p1t |> Fn.flip List.take p1h)
              ; p2 = Fqueue.of_list (Fqueue.to_list p2t |> Fn.flip List.take p2h)
              }
          with
          | `P1_victory, _ -> Fqueue.enqueue (Fqueue.enqueue p1t p1h) p2h, p2t
          | `P2_victory, _ -> p1t, Fqueue.enqueue (Fqueue.enqueue p2t p2h) p1h)
        else (
          (* Plain combat *)
          match Ordering.of_int (Int.compare p1h p2h) with
          | Greater -> Fqueue.enqueue (Fqueue.enqueue p1t p1h) p2h, p2t
          | Less -> p1t, Fqueue.enqueue (Fqueue.enqueue p2t p2h) p1h
          | Equal -> failwith "ambiguous")
      in
      ({ p1; p2 } : Input.t)
    | None, _ -> raise (Victory `P2)
    | _, None -> raise (Victory `P1)
  in
  let rec loop state states_already_encountered =
    if Set.mem states_already_encountered state
    then `P1_victory, state
    else (
      match round state with
      | exception Victory `P1 -> `P1_victory, state
      | exception Victory `P2 -> `P2_victory, state
      | state' -> loop state' (Set.add states_already_encountered state))
  in
  loop decks (Set.empty (module Input))
;;

let b () =
  let%bind decks = Lazy_deferred.force_exn Input.t in
  let score =
    match play_recursive decks with
    | `P1_victory, { p1; p2 = _ } -> score p1
    | `P2_victory, { p1 = _; p2 } -> score p2
  in
  print_s [%sexp (score : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 33661 |}] in
  return ()
;;
