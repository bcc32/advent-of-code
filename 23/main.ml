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

let a () =
  let%bind program = input () in
  let computers =
    Array.init 50 ~f:(fun i ->
      let program = Program.copy program in
      Program.Sync.provide_input program i;
      program)
  in
  let input_queues = Array.init 50 ~f:(fun _ -> Queue.create ()) in
  with_return (fun { return } ->
    while true do
      Array.iteri computers ~f:(fun i program ->
        match Program.Sync.step program with
        | Done -> ()
        | Need_input ->
          if Queue.is_empty input_queues.(i)
          then Program.Sync.provide_input program (-1)
          else Program.Sync.provide_input' program input_queues.(i)
        | Output dest ->
          let x = step_output_exn program in
          let y = step_output_exn program in
          if dest = 255
          then return y
          else Queue.enqueue_all input_queues.(dest) [ x; y ])
    done;
    assert false)
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 18192 |}]
;;

let b () =
  let%bind program = input () in
  let computers =
    Array.init 50 ~f:(fun i ->
      let program = Program.copy program in
      Program.Sync.provide_input program i;
      program)
  in
  let input_queues = Array.init 50 ~f:(fun _ -> Queue.create ()) in
  let nat = ref (-1, -1) in
  let delivered = ref [] in
  with_return (fun { return } ->
    while true do
      let is_idle = ref true in
      Array.iteri computers ~f:(fun i program ->
        match Program.Sync.step program with
        | Done -> ()
        | Need_input ->
          if Queue.is_empty input_queues.(i)
          then Program.Sync.provide_input program (-1)
          else (
            Program.Sync.provide_input' program input_queues.(i);
            is_idle := false)
        | Output dest ->
          let x = step_output_exn program in
          let y = step_output_exn program in
          if dest = 255
          then nat := x, y
          else (
            Queue.enqueue_all input_queues.(dest) [ x; y ];
            is_idle := false));
      if Array.exists input_queues ~f:(Fn.non Queue.is_empty) then is_idle := false;
      if !is_idle
      then (
        Queue.enqueue_all input_queues.(0) [ fst !nat; snd !nat ];
        delivered := !nat :: !delivered;
        match !delivered with
        | (_, y) :: (_, y') :: _ when y = y' -> return y
        | _ -> ())
    done;
    assert false)
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 10738 |}]
;;
