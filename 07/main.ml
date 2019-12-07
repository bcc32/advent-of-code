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
      let%bind input = input () in
      program.(addr) <- input;
      return (`Repeat (pc + 2))
    | 4 ->
      let arg1 = program.(pc + 1) in
      let x = get program ~arg:arg1 ~mode:mode1 in
      output x;
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
;;

let have_exn = function
  | `Eof -> raise_s [%message "EOF"]
  | `Ok x -> x
;;

let try_setting program a_setting b_setting c_setting d_setting e_setting =
  let a_output =
    let a_input = Pipe.of_list [ a_setting; 0 ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read a_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let b_output =
    let b_input = Pipe.concat [ Pipe.singleton b_setting; a_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read b_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let c_output =
    let c_input = Pipe.concat [ Pipe.singleton c_setting; b_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read c_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let d_output =
    let d_input = Pipe.concat [ Pipe.singleton d_setting; c_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read d_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let e_output =
    let e_input = Pipe.concat [ Pipe.singleton e_setting; d_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read e_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  Pipe.read e_output >>| have_exn
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
  let a_input_from_e_r, a_input_from_e_w = Pipe.create () in
  let a_output =
    let a_input = Pipe.concat [ Pipe.of_list [ a_setting; 0 ]; a_input_from_e_r ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read a_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let b_output =
    let b_input = Pipe.concat [ Pipe.singleton b_setting; a_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read b_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let c_output =
    let c_input = Pipe.concat [ Pipe.singleton c_setting; b_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read c_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let d_output =
    let d_input = Pipe.concat [ Pipe.singleton d_setting; c_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read d_input >>| have_exn)
        ~output:(Pipe.write_without_pushback writer))
  in
  let e_output =
    let e_input = Pipe.concat [ Pipe.singleton e_setting; d_output ] in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      main
        ~program:(Array.copy program)
        ~input:(fun () -> Pipe.read e_input >>| have_exn)
        ~output:(fun x ->
          Pipe.write_without_pushback writer x;
          Pipe.write_without_pushback a_input_from_e_w x))
  in
  Pipe.fold e_output ~init:0 ~f:(fun _ x -> return x)
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
