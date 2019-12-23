open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let step_output_exn program =
  match Program.Sync.step program with
  | Done | Need_input -> failwith "step_output_exn"
  | Output x -> x
;;

let run_network ~program ~on_nat_write ~on_idle =
  let computers =
    Array.init 50 ~f:(fun i ->
      let program = Program.copy program in
      Program.Sync.provide_input program i;
      program)
  in
  let input_queues = Array.init 50 ~f:(fun _ -> Queue.create ()) in
  let stop = ref false in
  let idle_rounds = ref 0 in
  let rec loop () =
    let is_idle = ref (Array.for_all input_queues ~f:Queue.is_empty) in
    Array.iteri computers ~f:(fun i program ->
      match Program.Sync.step program with
      | Done -> is_idle := false
      | Need_input ->
        if Queue.is_empty input_queues.(i)
        then Program.Sync.provide_input program (-1)
        else Program.Sync.provide_input' program input_queues.(i)
      | Output dst ->
        is_idle := false;
        let x = step_output_exn program in
        let y = step_output_exn program in
        if dst = 255
        then (
          match on_nat_write (x, y) with
          | `Stop -> stop := true
          | `Continue -> ())
        else Queue.enqueue_all input_queues.(dst) [ x; y ]);
    if !is_idle then incr idle_rounds;
    (* After we provide -1 as input to signal "no input", the program might then
       produce an output, so the system is not yet considered "idle".  If it
       happens twice in a row for all programs, then we must consider the system
       idle, since we have no way of guessing how many (-1)s as input it will
       take to get the programs to go again. *)
    if !idle_rounds >= 2
    then (
      (match on_idle ~input_queues with
       | `Stop -> stop := true
       | `Continue -> ());
      idle_rounds := 0);
    if not !stop then loop ()
  in
  loop ()
;;

let a () =
  let%bind program = input () in
  run_network
    ~program
    ~on_nat_write:(fun (_, y) ->
      printf "%d\n" y;
      `Stop)
    ~on_idle:(fun ~input_queues:_ -> `Continue);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 18192 |}]
;;

let b () =
  let%bind program = input () in
  let nat = Moption.create () in
  let last_delivered_y = Moption.create () in
  let answer = Set_once.create () in
  run_network
    ~program
    ~on_nat_write:(fun packet ->
      Moption.set_some nat packet;
      `Continue)
    ~on_idle:(fun ~input_queues ->
      let x, y = Moption.get_some_exn nat in
      Queue.enqueue_all input_queues.(0) [ x; y ];
      match Moption.get last_delivered_y with
      | Some y' when y = y' ->
        Set_once.set_exn answer [%here] y;
        `Stop
      | None | Some _ ->
        Moption.set_some last_delivered_y y;
        `Continue);
  printf "%d\n" (Set_once.get_exn answer [%here]);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 10738 |}]
;;
