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

let samples_and_program () =
  let%map samples_lines, program_lines =
    let%map contents = Reader.file_contents "aoc.in" in
    let i = String.substr_index_exn contents ~pattern:"\n\n\n\n" in
    let samples_lines = String.sub contents ~pos:0 ~len:i |> String.split_lines in
    let program_lines = String.subo contents ~pos:(i + 4) |> String.split_lines in
    samples_lines, program_lines
  in
  let samples =
    samples_lines
    |> List.group ~break:(fun _ s -> String.is_prefix s ~prefix:"Before")
    |> List.map ~f:(function
      | before :: insn :: after :: _ ->
        let before = parse_memory_state before in
        let insn = Insn.of_string insn in
        let after = parse_memory_state after in
        before, insn, after
      | _ -> assert false)
  in
  let program = List.map program_lines ~f:Insn.of_string in
  samples, program
;;

let main () =
  let%bind samples, _ = samples_and_program () in
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
  [%expect {| 529 |}];
  return ()
;;
