open! Core
open! Async
open! Import
open Intcode

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
      (Program.run (Program.copy program) ~input:(fst input) ~output:(snd output));
    { input; output }
  ;;

  module Infix = struct
    let ( --> ) t1 t2 = don't_wait_for (Pipe.transfer_id (fst t1.output) (snd t2.input))
  end
end

open Amp.Infix

let try_setting program a_setting b_setting c_setting d_setting e_setting =
  let a_amp = Amp.create ~program ~setting:a_setting in
  let b_amp = Amp.create ~program ~setting:b_setting in
  let c_amp = Amp.create ~program ~setting:c_setting in
  let d_amp = Amp.create ~program ~setting:d_setting in
  let e_amp = Amp.create ~program ~setting:e_setting in
  a_amp --> b_amp;
  b_amp --> c_amp;
  c_amp --> d_amp;
  d_amp --> e_amp;
  Pipe.write_without_pushback (snd a_amp.input) 0;
  match%map Pipe.read (fst e_amp.output) with
  | `Eof -> assert false
  | `Ok x -> x
;;

let a () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
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
  a_amp --> b_amp;
  b_amp --> c_amp;
  c_amp --> d_amp;
  d_amp --> e_amp;
  let e_output_1, e_output_2 =
    Pipe.fork (fst e_amp.output) ~pushback_uses:`Both_consumers
  in
  don't_wait_for (Pipe.transfer_id e_output_1 (snd a_amp.input));
  Pipe.write_without_pushback (snd a_amp.input) 0;
  Pipe.read_all e_output_2 >>| Queue.last_exn
;;

let b () =
  let%bind program = Reader.file_contents "input" >>| Program.of_string in
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
