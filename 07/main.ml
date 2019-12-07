open! Core
open! Async
open! Import

let program () =
  Reader.file_contents "input"
  >>| String.strip
  >>| String.split ~on:','
  >>| List.map ~f:Int.of_string
  >>| Array.of_list
;;

let decode instr =
  let opcode = instr % 100 in
  let mode1 = instr / 100 % 10 in
  let mode2 = instr / 1000 % 10 in
  let mode3 = instr / 10_000 % 10 in
  opcode, mode1, mode2, mode3
;;

let get program ~arg ~mode =
  match mode with
  | 0 -> program.(arg)
  | 1 -> arg
  | _ -> assert false
;;

let main ~input ~output ~program =
  let%bind () =
    Deferred.repeat_until_finished 0 (fun pc ->
      let opcode, mode1, mode2, _mode3 = decode program.(pc) in
      match opcode with
      | 1 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(program.(pc + 3)) <- x + y;
        return (`Repeat (pc + 4))
      | 2 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(program.(pc + 3)) <- x * y;
        return (`Repeat (pc + 4))
      | 3 ->
        let addr = program.(pc + 1) in
        (match%bind Pipe.read input with
         | `Eof -> raise_s [%message "program received EOF on input"]
         | `Ok input ->
           program.(addr) <- input;
           return (`Repeat (pc + 2)))
      | 4 ->
        let arg1 = program.(pc + 1) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let%bind () = Pipe.write output x in
        return (`Repeat (pc + 2))
      | 5 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        if x <> 0 then return (`Repeat y) else return (`Repeat (pc + 3))
      | 6 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        if x = 0 then return (`Repeat y) else return (`Repeat (pc + 3))
      | 7 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let arg3 = program.(pc + 3) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(arg3) <- Bool.to_int (x < y);
        return (`Repeat (pc + 4))
      | 8 ->
        let arg1 = program.(pc + 1) in
        let arg2 = program.(pc + 2) in
        let arg3 = program.(pc + 3) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(arg3) <- Bool.to_int (x = y);
        return (`Repeat (pc + 4))
      | 99 -> return (`Finished ())
      | code -> raise_s [%message "unrecognized opcode" (code : int)])
  in
  Pipe.close_read input;
  Pipe.close output;
  return ()
;;

let have_exn = function
  | `Eof -> raise_s [%message "EOF"]
  | `Ok x -> x
;;

module Amp = struct
  type t =
    { input : int Pipe.Reader.t * int Pipe.Writer.t
    ; output : int Pipe.Reader.t * int Pipe.Writer.t
    }

  let create ~program ~setting =
    let input = Pipe.create () in
    let output = Pipe.create () in
    Pipe.write_without_pushback (snd input) setting;
    don't_wait_for
      (main ~program:(Array.copy program) ~input:(fst input) ~output:(snd output));
    { input; output }
  ;;

  module Infix = struct
    let ( ||| ) t1 t2 = don't_wait_for (Pipe.transfer_id (fst t1.output) (snd t2.input))
  end
end

open Amp.Infix

let try_setting program a_setting b_setting c_setting d_setting e_setting =
  let a_amp = Amp.create ~program ~setting:a_setting in
  let b_amp = Amp.create ~program ~setting:b_setting in
  let c_amp = Amp.create ~program ~setting:c_setting in
  let d_amp = Amp.create ~program ~setting:d_setting in
  let e_amp = Amp.create ~program ~setting:e_setting in
  a_amp ||| b_amp;
  b_amp ||| c_amp;
  c_amp ||| d_amp;
  d_amp ||| e_amp;
  Pipe.write_without_pushback (snd a_amp.input) 0;
  match%map Pipe.read (fst e_amp.output) with
  | `Eof -> assert false
  | `Ok x -> x
;;

let a () =
  let%bind program = program () in
  let%bind best =
    Sequence.range 0 99_999 ~stop:`inclusive
    |> Sequence.filter_map ~f:(fun i ->
      let a = i % 10 in
      let b = i / 10 % 10 in
      let c = i / 100 % 10 in
      let d = i / 1000 % 10 in
      let e = i / 10_000 % 10 in
      let is_ok x = 0 <= x && x < 5 in
      if is_ok a
      && is_ok b
      && is_ok c
      && is_ok d
      && is_ok e
      && a <> b
      && a <> c
      && a <> d
      && a <> e
      && b <> c
      && b <> d
      && b <> e
      && c <> d
      && c <> e
      && d <> e
      then Some (a, b, c, d, e)
      else None)
    |> Sequence.map ~f:(fun (a, b, c, d, e) -> try_setting program a b c d e)
    |> Deferred.Sequence.all
    >>| Sequence.max_elt ~compare:[%compare: int]
    >>| uw
  in
  printf "%d\n" best;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 34852 |}]
;;

let try_setting program a_setting b_setting c_setting d_setting e_setting =
  let a_amp = Amp.create ~program ~setting:a_setting in
  let b_amp = Amp.create ~program ~setting:b_setting in
  let c_amp = Amp.create ~program ~setting:c_setting in
  let d_amp = Amp.create ~program ~setting:d_setting in
  let e_amp = Amp.create ~program ~setting:e_setting in
  a_amp ||| b_amp;
  b_amp ||| c_amp;
  c_amp ||| d_amp;
  d_amp ||| e_amp;
  let e_output_1, e_output_2 =
    Pipe.fork (fst e_amp.output) ~pushback_uses:`Both_consumers
  in
  don't_wait_for (Pipe.transfer_id e_output_1 (snd a_amp.input));
  Pipe.write_without_pushback (snd a_amp.input) 0;
  Pipe.read_all e_output_2 >>| Queue.last_exn
;;

let b () =
  let%bind program = program () in
  let%bind best =
    Sequence.range 0 99_999 ~stop:`inclusive
    |> Sequence.filter_map ~f:(fun i ->
      let a = i % 10 in
      let b = i / 10 % 10 in
      let c = i / 100 % 10 in
      let d = i / 1000 % 10 in
      let e = i / 10_000 % 10 in
      let is_ok x = 5 <= x && x <= 9 in
      if is_ok a
      && is_ok b
      && is_ok c
      && is_ok d
      && is_ok e
      && a <> b
      && a <> c
      && a <> d
      && a <> e
      && b <> c
      && b <> d
      && b <> e
      && c <> d
      && c <> e
      && d <> e
      then Some (a, b, c, d, e)
      else None)
    |> Sequence.map ~f:(fun (a, b, c, d, e) -> try_setting program a b c d e)
    |> Deferred.Sequence.all
    >>| Sequence.max_elt ~compare:[%compare: int]
    >>| uw
  in
  printf "%d\n" best;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 44282086 |}]
;;
