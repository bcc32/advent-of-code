open! Core
open! Async
open! Import

(* Label each spot with an index:

   {v
     #############
     #01.2.3.4.56#
     ###7#8#9#A###
       #B#C#D#E#
       #########
   v}

   We don't have to actually keep track of the spaces outside each room because
   they will always be clear.

   We also only need to track edges between room nodes and hallway nodes, and
   not intra-hallway nodes.
*)

let blank_map =
  {|
#############
#...........#
###.#.#.#.###
  #.#.#.#.#
  #########
|}
  |> String.strip
  |> String.split_lines
  |> List.to_array
;;

let target_state = ".......ABCDABCD"

module Node = struct
  (* coordinates relative to the blank string map *)
  type t =
    { row : int
    ; col : int
    }
  [@@deriving compare, hash, sexp_of]

  let get_blank t = blank_map.(t.row).[t.col]

  let all =
    [| 1, 1
     ; 1, 2
     ; 1, 4
     ; 1, 6
     ; 1, 8
     ; 1, 10
     ; 1, 11
     ; 2, 3
     ; 2, 5
     ; 2, 7
     ; 2, 9
     ; 3, 3
     ; 3, 5
     ; 3, 7
     ; 3, 9
    |]
    |> Array.map ~f:(fun (row, col) -> { row; col })
  ;;
end

module Edge = struct
  type t =
    { from : int
    ; to_ : int
    }

  let all =
    Array.concat_mapi Node.all ~f:(fun i from ->
      Array.concat_mapi Node.all ~f:(fun j to_ ->
        (* Rule out intra-hallway moves.  Also, we don't need to allow
           room-to-room moves since we can always pass through at least one
           hallway node. *)
        if from.row = to_.row || (from.row > 1 && to_.row > 1)
        then [||]
        else [| { from = i; to_ = j } |]))
  ;;
end

module Edge_rich = struct
  type t =
    { from : int
    ; to_ : int
    ; steps : int
    ; can_be_blocked_by : int list
    ; must_be_empty_or_same_letter : int option
    ; i_must_be_letter : char option
    }
  [@@deriving sexp_of]

  let bfs ~from ~to_ =
    let q = Queue.of_list [ Node.all.(from) ] in
    let dist = Hashtbl.of_alist_exn (module Node) [ Node.all.(from), (0, []) ] in
    while not (Queue.is_empty q) do
      let x = Queue.dequeue_exn q in
      let d, blocked = Hashtbl.find_exn dist x in
      let blocked =
        match Array.findi Node.all ~f:(fun _ x' -> [%compare.equal: Node.t] x x') with
        | Some (i, _) when i <> from -> i :: blocked
        | _ -> blocked
      in
      List.iter
        [ -1, 0; 1, 0; 0, -1; 0, 1 ]
        ~f:(fun (dr, dc) ->
          let next : Node.t = { row = x.row + dr; col = x.col + dc } in
          match Node.get_blank next with
          | '.' ->
            ignore
              (Hashtbl.find_or_add dist next ~default:(fun () ->
                 Queue.enqueue q next;
                 d + 1, blocked)
               : int * int list)
          | _ -> ())
    done;
    Hashtbl.find_exn dist Node.all.(to_)
  ;;

  let all =
    lazy
      (Array.map Edge.all ~f:(fun { from; to_ } ->
         let steps, can_be_blocked_by = bfs ~from ~to_ in
         let must_be_empty_or_same_letter =
           match Node.all.(to_).row with
           | 2 -> Some (to_ + 4)
           | 3 ->
             None
             (*  actually not needed because [can_be_blocked_by] will achieve the
                 same purpose *)
           | _ -> None
         in
         let i_must_be_letter =
           match Node.all.(to_).row with
           | 2 | 3 ->
             (match Node.all.(to_).col with
              | 3 -> Some 'A'
              | 5 -> Some 'B'
              | 7 -> Some 'C'
              | 9 -> Some 'D'
              | _ -> None)
           | _ -> None
         in
         { from
         ; to_
         ; steps
         ; can_be_blocked_by
         ; must_be_empty_or_same_letter
         ; i_must_be_letter
         }))
  ;;
end

let input =
  Lazy_deferred.create (fun () ->
    let%map lines =
      Reader.file_contents "aoc.in" >>| String.split_lines >>| List.to_array
    in
    Node.all |> Array.map ~f:(fun { row; col } -> lines.(row).[col]) |> String.of_array)
;;

let energy_per_step char =
  match char with
  | 'A' -> 1
  | 'B' -> 10
  | 'C' -> 100
  | 'D' -> 1000
  | c -> failwithf "unrecognized char %c" c ()
;;

let possible_transitions state =
  let ans = ref [] in
  Array.iter
    (force Edge_rich.all)
    ~f:
      (fun
        { from
        ; to_
        ; steps
        ; can_be_blocked_by
        ; must_be_empty_or_same_letter
        ; i_must_be_letter
        }
      ->
      match state.[from] with
      | 'A' .. 'D' as char ->
        let energy = steps * energy_per_step char in
        if List.for_all can_be_blocked_by ~f:(fun i -> Char.equal state.[i] '.')
           && Option.for_all must_be_empty_or_same_letter ~f:(fun i ->
             Char.equal state.[i] '.' || Char.equal state.[i] char)
           && Option.for_all i_must_be_letter ~f:(Char.equal char)
        then (
          let new_state = Bytes.of_string state in
          Bytes.set new_state from '.';
          Bytes.set new_state to_ char;
          ans
          := (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:new_state, energy)
             :: !ans)
      | _ -> ());
  !ans
;;

let a_star initial_state =
  let queue = Queue.create () in
  let cost = Hashtbl.create (module String) in
  let swap i j =
    let t = Queue.get queue i in
    Queue.set queue i (Queue.get queue j);
    Queue.set queue j t
  in
  let dequeue_min_by ~f =
    let _, i =
      Queue.foldi queue ~init:(Int.max_value, -1) ~f:(fun i' (accum_cost, i) state ->
        let c = f state in
        if c < accum_cost then c, i' else accum_cost, i)
    in
    assert (i >= 0);
    swap i 0;
    Queue.dequeue_exn queue
  in
  Queue.enqueue queue initial_state;
  Hashtbl.add_exn cost ~key:initial_state ~data:0;
  with_return (fun { return } ->
    while not (Queue.is_empty queue) do
      let state =
        dequeue_min_by ~f:(fun state ->
          Hashtbl.find_exn cost state
          + (String.counti state ~f:(fun i x -> Char.( <> ) target_state.[i] x) / 2))
      in
      let c = Hashtbl.find_exn cost state in
      if String.equal state target_state then return c;
      List.iter (possible_transitions state) ~f:(fun (state', dcost) ->
        let should_add =
          match Hashtbl.find cost state' with
          | None -> true
          | Some c' when c' > dcost + c ->
            Hashtbl.set cost ~key:state' ~data:(dcost + c);
            false
          | Some _ -> false
        in
        if should_add
        then (
          Queue.enqueue queue state';
          Hashtbl.set cost ~key:state' ~data:(dcost + c)))
    done;
    raise_s [%message "no solution"])
;;

let a () =
  let%bind initial_state = Lazy_deferred.force_exn input in
  print_s [%message (possible_transitions initial_state : (string * int) list)];
  let ans = a_star initial_state in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  return ()
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (monitor.ml.Error "no solution"
    ("Raised at Base__Error.raise in file \"src/error.ml\" (inlined), line 9, characters 14-30"
      "Called from Base__Error.raise_s in file \"src/error.ml\", line 10, characters 19-40"
      "Called from Base__With_return.with_return in file \"src/with_return.ml\", line 21, characters 12-24"
      "Re-raised at Base__With_return.with_return in file \"src/with_return.ml\", line 29, characters 12-21"
      "Called from Year2021_p23__Main.a_star in file \"2021/23/main.ml\", line 202, characters 2-677"
      "Called from Async_kernel__Deferred0.bind.(fun) in file \"src/deferred0.ml\", line 54, characters 64-69"
      "Called from Async_kernel__Job_queue.run_jobs in file \"src/job_queue.ml\", line 180, characters 6-47"
      "Caught by monitor block_on_async"))
  Raised at Base__Result.ok_exn in file "src/result.ml", line 251, characters 17-26
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  (edge
   ((from 7) (to_ 0) (steps 3) (can_be_blocked_by (1 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 1) (steps 2) (can_be_blocked_by (7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 2) (steps 2) (can_be_blocked_by (7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 3) (steps 4) (can_be_blocked_by (2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 4) (steps 6) (can_be_blocked_by (3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 5) (steps 8) (can_be_blocked_by (4 3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 6) (steps 9) (can_be_blocked_by (5 4 3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 0) (steps 5) (can_be_blocked_by (1 2 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 1) (steps 4) (can_be_blocked_by (2 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 2) (steps 2) (can_be_blocked_by (8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 3) (steps 2) (can_be_blocked_by (8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 4) (steps 4) (can_be_blocked_by (3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 5) (steps 6) (can_be_blocked_by (4 3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 6) (steps 7) (can_be_blocked_by (5 4 3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 0) (steps 7) (can_be_blocked_by (1 2 3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 1) (steps 6) (can_be_blocked_by (2 3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 2) (steps 4) (can_be_blocked_by (3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 3) (steps 2) (can_be_blocked_by (9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 4) (steps 2) (can_be_blocked_by (9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 5) (steps 4) (can_be_blocked_by (4 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 6) (steps 5) (can_be_blocked_by (5 4 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 0) (steps 9) (can_be_blocked_by (1 2 3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 1) (steps 8) (can_be_blocked_by (2 3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 2) (steps 6) (can_be_blocked_by (3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 3) (steps 4) (can_be_blocked_by (4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 4) (steps 2) (can_be_blocked_by (10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 5) (steps 2) (can_be_blocked_by (10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 6) (steps 3) (can_be_blocked_by (5 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 0) (steps 4) (can_be_blocked_by (1 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 1) (steps 3) (can_be_blocked_by (7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 2) (steps 3) (can_be_blocked_by (7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 3) (steps 5) (can_be_blocked_by (2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 4) (steps 7) (can_be_blocked_by (3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 5) (steps 9) (can_be_blocked_by (4 3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 6) (steps 10) (can_be_blocked_by (5 4 3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 0) (steps 6) (can_be_blocked_by (1 2 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 1) (steps 5) (can_be_blocked_by (2 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 2) (steps 3) (can_be_blocked_by (8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 3) (steps 3) (can_be_blocked_by (8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 4) (steps 5) (can_be_blocked_by (3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 5) (steps 7) (can_be_blocked_by (4 3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 6) (steps 8) (can_be_blocked_by (5 4 3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 0) (steps 8) (can_be_blocked_by (1 2 3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 1) (steps 7) (can_be_blocked_by (2 3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 2) (steps 5) (can_be_blocked_by (3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 3) (steps 3) (can_be_blocked_by (9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 4) (steps 3) (can_be_blocked_by (9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 5) (steps 5) (can_be_blocked_by (4 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 6) (steps 6) (can_be_blocked_by (5 4 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 0) (steps 10) (can_be_blocked_by (1 2 3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 1) (steps 9) (can_be_blocked_by (2 3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 2) (steps 7) (can_be_blocked_by (3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 3) (steps 5) (can_be_blocked_by (4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 4) (steps 3) (can_be_blocked_by (10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 5) (steps 3) (can_be_blocked_by (10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 6) (steps 4) (can_be_blocked_by (5 10 14))
    (must_be_empty_or_same_letter ())))
  ("possible_transitions initial_state" ())
  (edge
   ((from 7) (to_ 0) (steps 3) (can_be_blocked_by (1 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 1) (steps 2) (can_be_blocked_by (7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 2) (steps 2) (can_be_blocked_by (7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 3) (steps 4) (can_be_blocked_by (2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 4) (steps 6) (can_be_blocked_by (3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 5) (steps 8) (can_be_blocked_by (4 3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 7) (to_ 6) (steps 9) (can_be_blocked_by (5 4 3 2 7))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 0) (steps 5) (can_be_blocked_by (1 2 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 1) (steps 4) (can_be_blocked_by (2 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 2) (steps 2) (can_be_blocked_by (8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 3) (steps 2) (can_be_blocked_by (8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 4) (steps 4) (can_be_blocked_by (3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 5) (steps 6) (can_be_blocked_by (4 3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 8) (to_ 6) (steps 7) (can_be_blocked_by (5 4 3 8))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 0) (steps 7) (can_be_blocked_by (1 2 3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 1) (steps 6) (can_be_blocked_by (2 3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 2) (steps 4) (can_be_blocked_by (3 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 3) (steps 2) (can_be_blocked_by (9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 4) (steps 2) (can_be_blocked_by (9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 5) (steps 4) (can_be_blocked_by (4 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 9) (to_ 6) (steps 5) (can_be_blocked_by (5 4 9))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 0) (steps 9) (can_be_blocked_by (1 2 3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 1) (steps 8) (can_be_blocked_by (2 3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 2) (steps 6) (can_be_blocked_by (3 4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 3) (steps 4) (can_be_blocked_by (4 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 4) (steps 2) (can_be_blocked_by (10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 5) (steps 2) (can_be_blocked_by (10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 10) (to_ 6) (steps 3) (can_be_blocked_by (5 10))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 0) (steps 4) (can_be_blocked_by (1 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 1) (steps 3) (can_be_blocked_by (7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 2) (steps 3) (can_be_blocked_by (7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 3) (steps 5) (can_be_blocked_by (2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 4) (steps 7) (can_be_blocked_by (3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 5) (steps 9) (can_be_blocked_by (4 3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 11) (to_ 6) (steps 10) (can_be_blocked_by (5 4 3 2 7 11))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 0) (steps 6) (can_be_blocked_by (1 2 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 1) (steps 5) (can_be_blocked_by (2 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 2) (steps 3) (can_be_blocked_by (8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 3) (steps 3) (can_be_blocked_by (8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 4) (steps 5) (can_be_blocked_by (3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 5) (steps 7) (can_be_blocked_by (4 3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 12) (to_ 6) (steps 8) (can_be_blocked_by (5 4 3 8 12))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 0) (steps 8) (can_be_blocked_by (1 2 3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 1) (steps 7) (can_be_blocked_by (2 3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 2) (steps 5) (can_be_blocked_by (3 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 3) (steps 3) (can_be_blocked_by (9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 4) (steps 3) (can_be_blocked_by (9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 5) (steps 5) (can_be_blocked_by (4 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 13) (to_ 6) (steps 6) (can_be_blocked_by (5 4 9 13))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 0) (steps 10) (can_be_blocked_by (1 2 3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 1) (steps 9) (can_be_blocked_by (2 3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 2) (steps 7) (can_be_blocked_by (3 4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 3) (steps 5) (can_be_blocked_by (4 10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 4) (steps 3) (can_be_blocked_by (10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 5) (steps 3) (can_be_blocked_by (10 14))
    (must_be_empty_or_same_letter ())))
  (edge
   ((from 14) (to_ 6) (steps 4) (can_be_blocked_by (5 10 14))
    (must_be_empty_or_same_letter ()))) |}]
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  print_s [%sexp (String.length input : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 15 |}];
  return ()
;;
