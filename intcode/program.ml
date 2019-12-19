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

let decode instr =
  let opcode = instr % 100 in
  let mode1 = instr / 100 % 10 in
  let mode2 = instr / 1000 % 10 in
  let mode3 = instr / 10_000 % 10 in
  opcode, mode1, mode2, mode3
;;

let get t ~arg ~mode =
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
  | 1 -> failwith "Cannot set using addressing mode 1"
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

  let rec step ({ memory; relative_base; pc; input } as t) : Step_result.t =
    let opcode, mode1, mode2, mode3 = decode memory.(pc) in
    let with1 f =
      let arg1 = memory.(pc + 1) in
      f ~x:(get t ~arg:arg1 ~mode:mode1)
    in
    let with2 f =
      let arg1 = memory.(pc + 1) in
      let arg2 = memory.(pc + 2) in
      f ~x:(get t ~arg:arg1 ~mode:mode1) ~y:(get t ~arg:arg2 ~mode:mode2)
    in
    let with3 f =
      let arg1 = memory.(pc + 1) in
      let arg2 = memory.(pc + 2) in
      let arg3 = memory.(pc + 3) in
      f ~x:(get t ~arg:arg1 ~mode:mode1) ~y:(get t ~arg:arg2 ~mode:mode2) ~arg3 ~mode3
    in
    match opcode with
    | 1 ->
      with3 (fun ~x ~y ~arg3 ~mode3 -> set t ~arg:arg3 ~mode:mode3 ~value:(x + y));
      t.pc <- pc + 4;
      step t
    | 2 ->
      with3 (fun ~x ~y ~arg3 ~mode3 -> set t ~arg:arg3 ~mode:mode3 ~value:(x * y));
      t.pc <- pc + 4;
      step t
    | 3 ->
      let arg1 = memory.(pc + 1) in
      (match Queue.dequeue input with
       | None -> Need_input
       | Some input ->
         set t ~arg:arg1 ~mode:mode1 ~value:input;
         t.pc <- pc + 2;
         step t)
    | 4 ->
      let x = with1 (fun ~x -> x) in
      t.pc <- pc + 2;
      Output x
    | 5 ->
      t.pc <- with2 (fun ~x ~y -> if x <> 0 then y else pc + 3);
      step t
    | 6 ->
      t.pc <- with2 (fun ~x ~y -> if x = 0 then y else pc + 3);
      step t
    | 7 ->
      with3 (fun ~x ~y ~arg3 ~mode3 ->
        set t ~arg:arg3 ~mode:mode3 ~value:(Bool.to_int (x < y)));
      t.pc <- pc + 4;
      step t
    | 8 ->
      with3 (fun ~x ~y ~arg3 ~mode3 ->
        set t ~arg:arg3 ~mode:mode3 ~value:(Bool.to_int (x = y)));
      t.pc <- pc + 4;
      step t
    | 9 ->
      with1 (fun ~x -> t.relative_base <- relative_base + x);
      t.pc <- pc + 2;
      step t
    | 99 -> Done
    | code -> raise_s [%message "unrecognized opcode" (code : int) (t : t)]
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
