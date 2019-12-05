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
  let pc = ref 0 in
  (* program.(1) <- 12;
   * program.(2) <- 2; *)
  try
    while true do
      let opcode, mode1, mode2, _mode3 = decode program.(!pc) in
      match opcode with
      | 1 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(program.(!pc + 3)) <- x + y;
        pc := !pc + 4
      | 2 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(program.(!pc + 3)) <- x * y;
        pc := !pc + 4
      | 3 ->
        let addr = program.(!pc + 1) in
        let input = input () in
        program.(addr) <- input;
        pc := !pc + 2
      | 4 ->
        let arg1 = program.(!pc + 1) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        output x;
        pc := !pc + 2
      | 5 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        if x <> 0 then pc := y else pc := !pc + 3
      | 6 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        if x = 0 then pc := y else pc := !pc + 3
      | 7 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let arg3 = program.(!pc + 3) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(arg3) <- Bool.to_int (x < y);
        pc := !pc + 4
      | 8 ->
        let arg1 = program.(!pc + 1) in
        let arg2 = program.(!pc + 2) in
        let arg3 = program.(!pc + 3) in
        let x = get program ~arg:arg1 ~mode:mode1 in
        let y = get program ~arg:arg2 ~mode:mode2 in
        program.(arg3) <- Bool.to_int (x = y);
        pc := !pc + 4
      | 99 -> raise Exit
      | _ -> ()
    done;
    assert false
  with
  | Exit -> ()
;;

let a () =
  let%bind program = program () in
  main ~program ~input:(Fn.const 1) ~output:(printf "%d\n");
  return ()
;;

let%expect_test "a" =
  Backtrace.elide := false;
  let%bind () = a () in
  [%expect {|
    0
    0
    0
    0
    0
    0
    0
    0
    0
    15426686 |}]
;;

let b () =
  let%bind program = program () in
  main ~program ~input:(Fn.const 5) ~output:(printf "%d\n");
  return ()
;;


let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 11430197 |}]
;;
