open! Core
open! Async
open! Import

let parse_memory_state =
  let rex =
    let open Re in
    let num = rep1 digit in
    compile
      (seq [ group num; str ", "; group num; str ", "; group num; str ", "; group num ])
  in
  fun line ->
    let group = Re.exec rex line in
    Re.Group.all group |> Array.subo ~pos:1 |> Array.map ~f:Int.of_string
;;

let input () =
  let%map lines = Reader.file_lines "input1" in
  lines
  |> List.group ~break:(fun _ s -> String.is_prefix s ~prefix:"Before")
  |> List.map ~f:(function
    | before :: insn :: after :: _ ->
      let before = parse_memory_state before in
      let insn = Insn.of_string insn in
      let after = parse_memory_state after in
      before, insn, after
    | _ -> assert false)
;;

let main () =
  let%bind samples = input () in
  List.count samples ~f:(fun (before, insn, after) ->
    List.count Insn.Opcode.all ~f:(fun opcode ->
      let memory = Array.copy before in
      Insn.exec insn ~memory ~op:(Fn.const opcode);
      [%equal: int array] memory after)
    >= 3)
  |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 529 |}]
;;
