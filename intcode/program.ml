open! Core
open! Async
open! Import

module Run = struct
  type t =
    { input : int Pipe.Writer.t
    ; output : int Pipe.Reader.t
    ; done_ : unit Deferred.t
    }
end

type t =
  { mutable memory : int array
  ; mutable relative_base : int
  }
[@@deriving sexp_of]

let of_string input =
  let memory =
    input |> String.strip |> String.split ~on:',' |> Array.of_list_map ~f:Int.of_string
  in
  { memory; relative_base = 0 }
;;

let copy t = { t with memory = Array.copy t.memory }

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

let run t =
  let input_r, input_w = Pipe.create () in
  let output_r, output_w = Pipe.create () in
  let done_ =
    Deferred.repeat_until_finished 0 (fun pc ->
      let opcode, mode1, mode2, mode3 = decode t.memory.(pc) in
      let with1 f =
        let arg1 = t.memory.(pc + 1) in
        f ~x:(get t ~arg:arg1 ~mode:mode1)
      in
      let with2 f =
        let arg1 = t.memory.(pc + 1) in
        let arg2 = t.memory.(pc + 2) in
        f ~x:(get t ~arg:arg1 ~mode:mode1) ~y:(get t ~arg:arg2 ~mode:mode2)
      in
      let with3 f =
        let arg1 = t.memory.(pc + 1) in
        let arg2 = t.memory.(pc + 2) in
        let arg3 = t.memory.(pc + 3) in
        f ~x:(get t ~arg:arg1 ~mode:mode1) ~y:(get t ~arg:arg2 ~mode:mode2) ~arg3 ~mode3
      in
      match opcode with
      | 1 ->
        with3 (fun ~x ~y ~arg3 ~mode3 -> set t ~arg:arg3 ~mode:mode3 ~value:(x + y));
        return (`Repeat (pc + 4))
      | 2 ->
        with3 (fun ~x ~y ~arg3 ~mode3 -> set t ~arg:arg3 ~mode:mode3 ~value:(x * y));
        return (`Repeat (pc + 4))
      | 3 ->
        let arg1 = t.memory.(pc + 1) in
        (match%bind Pipe.read input_r with
         | `Eof -> raise_s [%message "t received EOF on input"]
         | `Ok input ->
           set t ~arg:arg1 ~mode:mode1 ~value:input;
           return (`Repeat (pc + 2)))
      | 4 ->
        let x = with1 (fun ~x -> x) in
        let%bind () = Pipe.write output_w x in
        return (`Repeat (pc + 2))
      | 5 ->
        let pc = with2 (fun ~x ~y -> if x <> 0 then y else pc + 3) in
        return (`Repeat pc)
      | 6 ->
        let pc = with2 (fun ~x ~y -> if x = 0 then y else pc + 3) in
        return (`Repeat pc)
      | 7 ->
        with3 (fun ~x ~y ~arg3 ~mode3 ->
          set t ~arg:arg3 ~mode:mode3 ~value:(Bool.to_int (x < y)));
        return (`Repeat (pc + 4))
      | 8 ->
        with3 (fun ~x ~y ~arg3 ~mode3 ->
          set t ~arg:arg3 ~mode:mode3 ~value:(Bool.to_int (x = y)));
        return (`Repeat (pc + 4))
      | 9 ->
        with1 (fun ~x -> t.relative_base <- t.relative_base + x);
        return (`Repeat (pc + 2))
      | 99 -> return (`Finished ())
      | code -> raise_s [%message "unrecognized opcode" (code : int)])
  in
  don't_wait_for
    (let%map () = done_ in
     Pipe.close_read input_r;
     Pipe.close output_w);
  { Run.input = input_w; output = output_r; done_ }
;;

let run_without_io t =
  let { Run.input; output; done_ } = run t in
  Pipe.close input;
  Pipe.close_read output;
  done_
;;
