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

let run t ~input ~output =
  let%bind () =
    Deferred.repeat_until_finished 0 (fun pc ->
      let opcode, mode1, mode2, _mode3 = decode t.(pc) in
      match opcode with
      | 1 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        t.(t.(pc + 3)) <- x + y;
        return (`Repeat (pc + 4))
      | 2 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        t.(t.(pc + 3)) <- x * y;
        return (`Repeat (pc + 4))
      | 3 ->
        let addr = t.(pc + 1) in
        (match%bind Pipe.read input with
         | `Eof -> raise_s [%message "t received EOF on input"]
         | `Ok input ->
           t.(addr) <- input;
           return (`Repeat (pc + 2)))
      | 4 ->
        let arg1 = t.(pc + 1) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let%bind () = Pipe.write output x in
        return (`Repeat (pc + 2))
      | 5 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        if x <> 0 then return (`Repeat y) else return (`Repeat (pc + 3))
      | 6 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        if x = 0 then return (`Repeat y) else return (`Repeat (pc + 3))
      | 7 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let arg3 = t.(pc + 3) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        t.(arg3) <- Bool.to_int (x < y);
        return (`Repeat (pc + 4))
      | 8 ->
        let arg1 = t.(pc + 1) in
        let arg2 = t.(pc + 2) in
        let arg3 = t.(pc + 3) in
        let x = get t ~arg:arg1 ~mode:mode1 in
        let y = get t ~arg:arg2 ~mode:mode2 in
        t.(arg3) <- Bool.to_int (x = y);
        return (`Repeat (pc + 4))
      | 99 -> return (`Finished ())
      | code -> raise_s [%message "unrecognized opcode" (code : int)])
  in
  Pipe.close_read input;
  Pipe.close output;
  return ()
;;
