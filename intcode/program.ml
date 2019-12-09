open! Core
open! Async
open! Import

type t = int array [@@deriving sexp_of]

let of_string input =
  input |> String.strip |> String.split ~on:',' |> Array.of_list_map ~f:Int.of_string
;;

let copy = Array.copy

let decode instr =
  let opcode = instr % 100 in
  let mode1 = instr / 100 % 10 in
  let mode2 = instr / 1000 % 10 in
  let mode3 = instr / 10_000 % 10 in
  opcode, mode1, mode2, mode3
;;

let get t ~arg ~mode =
  match mode with
  | 0 -> t.(arg)
  | 1 -> arg
  | _ -> assert false
;;

let set t ~arg ~mode ~value =
  match mode with
  | 0 -> t.(arg) <- value
  | 1 -> failwith "Cannot set using addressing mode 1"
  | _ -> assert false
;;

let run t ~input ~output =
  let%bind () =
    Deferred.repeat_until_finished 0 (fun pc ->
      let opcode, mode1, mode2, mode3 = decode t.(pc) in
      let with1 f =
        let arg1 = t.(pc + 1) in
        f ~x:(get t ~arg:arg1 ~mode:mode1)
      in
      let with2 f =
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        f ~x:(get t ~arg:arg1 ~mode:mode1) ~y:(get t ~arg:arg2 ~mode:mode2)
      in
      let with3 f =
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let arg3 = t.(pc + 3) in
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
        let arg1 = t.(pc + 1) in
        (match%bind Pipe.read input with
         | `Eof -> raise_s [%message "t received EOF on input"]
         | `Ok input ->
           set t ~arg:arg1 ~mode:mode1 ~value:input;
           return (`Repeat (pc + 2)))
      | 4 ->
        let x = with1 (fun ~x -> x) in
        let%bind () = Pipe.write output x in
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
      | 99 -> return (`Finished ())
      | code -> raise_s [%message "unrecognized opcode" (code : int)])
  in
  Pipe.close_read input;
  Pipe.close output;
  return ()
;;

let run_without_io t =
  run
    t
    ~input:(Pipe.of_list [])
    ~output:
      (Pipe.create_writer (fun r ->
         Pipe.close_read r;
         return ()))
;;
