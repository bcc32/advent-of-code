open! Core
open! Async
open! Import

type t =
  { mutable memory : int array
  ; mutable relative_base : int
  ; mutable pc : int
  ; mutable input : int Queue.t
  }
[@@deriving sexp_of]

let of_string source_code =
  let memory =
    source_code
    |> String.strip
    |> String.split ~on:','
    |> Array.of_list_map ~f:Int.of_string
  in
  { memory; relative_base = 0; pc = 0; input = Queue.create () }
;;

let copy t =
  if not (Queue.is_empty t.input)
  then raise_s [%message "Program.copy: tried to copy a program with pending input"];
  { t with memory = Array.copy t.memory; input = Queue.create () }
;;

let restore ~src ~dst =
  if (not (Queue.is_empty src.input)) || not (Queue.is_empty dst.input)
  then raise_s [%message "Program.restore: tried to restore a program with pending input"];
  let gap = Array.length dst.memory - Array.length src.memory in
  match Int.sign gap with
  | Neg -> dst.memory <- Array.copy src.memory
  | Zero | Pos ->
    Array.blit
      ~src:src.memory
      ~src_pos:0
      ~dst:dst.memory
      ~dst_pos:0
      ~len:(Int.min (Array.length src.memory) (Array.length dst.memory));
    if gap > 0 then Array.fill dst.memory 0 ~pos:(Array.length src.memory) ~len:gap;
    dst.pc <- src.pc;
    dst.relative_base <- src.relative_base
;;

module Insn = struct
  (* TODO: These divisions are actually kind of slow (they take up a lot of time in
     problem 19).  Perhaps we should just store instructions as strings? *)
  let[@inline always] opcode t = t % 100
  let[@inline always] mode1 t = t / 100 % 10
  let[@inline always] mode2 t = t / 1000 % 10
  let[@inline always] mode3 t = t / 10_000 % 10
end

let[@inline always] get t ~arg ~mode =
  let try_get index =
    try t.memory.(index) with
    | _ -> 0
  in
  match mode with
  | 0 -> try_get arg
  | 1 -> arg
  | 2 -> try_get (arg + t.relative_base)
  | _ -> assert false
;;

let[@cold] raise_set_invalid_addressing_mode ~mode =
  raise_s [%message "Cannot set using addressing mode" (mode : int)]
;;

let set t ~arg ~mode ~value =
  let grow index =
    let new_array =
      Array.create 0 ~len:(Int.max (index + 1) (Array.length t.memory * 2))
    in
    Array.blit
      ~src:t.memory
      ~dst:new_array
      ~src_pos:0
      ~dst_pos:0
      ~len:(Array.length t.memory);
    t.memory <- new_array
  in
  let try_set index value =
    try t.memory.(index) <- value with
    | _ ->
      grow index;
      t.memory.(index) <- value
  in
  match mode with
  | 0 -> try_set arg value
  | 1 -> raise_set_invalid_addressing_mode ~mode
  | 2 -> try_set (arg + t.relative_base) value
  | _ -> assert false
;;

module Sync = struct
  module Step_result = struct
    type t =
      | Done
      | Need_input
      | Output of int
  end

  let[@cold] raise_unrecognized_opcode t ~code =
    raise_s [%message "unrecognized opcode" (code : int) (t : t)]
  ;;

  let rec step ({ memory; relative_base; pc; input } as t) : Step_result.t =
    let insn = memory.(pc) in
    let[@inline always] x () =
      let arg = memory.(pc + 1) in
      get t ~arg ~mode:(Insn.mode1 insn)
    in
    let[@inline always] y () =
      let arg = memory.(pc + 2) in
      get t ~arg ~mode:(Insn.mode2 insn)
    in
    let[@inline always] set_z value =
      let arg = memory.(pc + 3) in
      set t ~arg ~mode:(Insn.mode3 insn) ~value
    in
    match Insn.opcode insn with
    | 1 ->
      set_z (x () + y ());
      t.pc <- pc + 4;
      step t
    | 2 ->
      set_z (x () * y ());
      t.pc <- pc + 4;
      step t
    | 3 ->
      let arg1 = memory.(pc + 1) in
      (match Queue.dequeue input with
       | None -> Need_input
       | Some input ->
         set t ~arg:arg1 ~mode:(Insn.mode1 insn) ~value:input;
         t.pc <- pc + 2;
         step t)
    | 4 ->
      let x = x () in
      t.pc <- pc + 2;
      Output x
    | 5 ->
      t.pc <- (if x () <> 0 then y () else pc + 3);
      step t
    | 6 ->
      t.pc <- (if x () = 0 then y () else pc + 3);
      step t
    | 7 ->
      set_z (Bool.to_int (x () < y ()));
      t.pc <- pc + 4;
      step t
    | 8 ->
      set_z (Bool.to_int (x () = y ()));
      t.pc <- pc + 4;
      step t
    | 9 ->
      t.relative_base <- relative_base + x ();
      t.pc <- pc + 2;
      step t
    | 99 -> Done
    | code -> raise_unrecognized_opcode t ~code
  ;;

  let run_without_input_exn t ~f =
    let rec loop () =
      match step t with
      | Done -> ()
      | Need_input -> raise_s [%message "Program.Sync.run_without_input_exn: need input"]
      | Output x ->
        f x;
        loop ()
    in
    loop ()
  ;;

  let provide_input' t input = Queue.blit_transfer () ~src:input ~dst:t.input
  let provide_input t input = Queue.enqueue t.input input
end

module Async = struct
  open Eager_deferred.Let_syntax

  module Run = struct
    type t =
      { input : int Pipe.Writer.t
      ; output : int Pipe.Reader.t
      ; done_ : unit Deferred.t
      }
  end

  let run t =
    let input_r, input_w = Pipe.create () in
    let output_r, output_w = Pipe.create () in
    let done_ =
      Deferred.repeat_until_finished () (fun () ->
        match Sync.step t with
        | Done -> return (`Finished ())
        | Need_input ->
          (match%map Pipe.read' input_r with
           | `Eof ->
             raise_s [%message "Program.Async.run: Program received EOF on input"]
           | `Ok queue ->
             Sync.provide_input' t queue;
             `Repeat ())
        | Output x ->
          let%map () = Pipe.write output_w x in
          `Repeat ())
    in
    don't_wait_for
      (let%map () = done_ in
       Pipe.close_read input_r;
       Pipe.close output_w);
    { Run.input = input_w; output = output_r; done_ }
  ;;
end
