open! Core
open! Async
open! Import

let main () =
  let%bind program =
    Reader.file_contents "input"
    >>| String.strip
    >>| String.split ~on:','
    >>| List.map ~f:Int.of_string
    >>| Array.of_list
  in
  let pc = ref 0 in
  program.(1) <- 12;
  program.(2) <- 2;
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
  | Exit ->
    printf !"%d\n" program.(0);
    return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 12490719 |}]
;;
