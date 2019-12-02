open! Core
open! Async
open! Import

let try_ program ~noun ~verb =
  let pc = ref 0 in
  program.(1) <- noun;
  program.(2) <- verb;
  try
    while true do
      (match program.(!pc) with
      | 1 ->
        let x = program.(program.(!pc + 1)) in
        let y = program.(program.(!pc + 2)) in
        program.(program.(!pc + 3)) <- x + y
      | 2 ->
        let x = program.(program.(!pc + 1)) in
        let y = program.(program.(!pc + 2)) in
        program.(program.(!pc + 3)) <- x * y
      | 99 -> raise Exit
      | _ -> ());
      pc := !pc + 4
    done;
    assert false
  with
  | Exit -> program.(0)
;;

let main () =
  let%bind program =
    Reader.file_contents "input"
    >>| String.strip
    >>| String.split ~on:','
    >>| List.map ~f:Int.of_string
    >>| Array.of_list
  in
  for noun = 0 to 99 do
    for verb = 0 to 99 do
      if try_ (Array.copy program) ~noun ~verb = 19690720
      then printf "%d\n" ((100 * noun) + verb)
    done
  done;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 2003 |}]
;;
