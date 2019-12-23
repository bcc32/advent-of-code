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
  let rec loop () =
    let is_idle = ref true in
    Array.iteri computers ~f:(fun i program ->
      match Program.Sync.step program with
      | Done -> ()
      | Need_input ->
        if Queue.is_empty input_queues.(i)
        then Program.Sync.provide_input program (-1)
        else (
          is_idle := false;
          Program.Sync.provide_input' program input_queues.(i))
      | Output dst ->
        let x = step_output_exn program in
        let y = step_output_exn program in
        if dst = 255
        then (
          match on_nat_write (x, y) with
          | `Stop -> stop := true
          | `Continue -> ())
        else Queue.enqueue_all input_queues.(dst) [ x; y ]);
    is_idle := !is_idle && Array.for_all input_queues ~f:Queue.is_empty;
    if !is_idle
    then (
      match on_idle ~input_queues with
      | `Stop -> stop := true
      | `Continue -> ());
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
  let nat = ref (-1, -1) in
  let delivered = ref [] in
  let answer = Set_once.create () in
  run_network
    ~program
    ~on_nat_write:(fun (x, y) ->
      nat := x, y;
      `Continue)
    ~on_idle:(fun ~input_queues ->
      Queue.enqueue_all input_queues.(0) [ fst !nat; snd !nat ];
      delivered := !nat :: !delivered;
      match !delivered with
      | (_, y) :: (_, y') :: _ when y = y' ->
        Set_once.set_exn answer [%here] y;
        `Stop
      | _ -> `Continue);
  printf "%d\n" (Set_once.get_exn answer [%here]);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 10738 |}]
;;
