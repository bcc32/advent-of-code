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
  let opint_to_opcode = Array.init 16 ~f:(Fn.const Insn.Opcode.all) in
  List.iter samples ~f:(fun (before, insn, after) ->
    let could_be_these_opcodes =
      List.filter Insn.Opcode.all ~f:(fun opcode ->
        let memory = Array.copy before in
        Insn.exec insn ~memory ~op:(Fn.const opcode);
        [%equal: int array] memory after)
    in
    opint_to_opcode.(insn.opcode)
    <- List.filter opint_to_opcode.(insn.opcode) ~f:(fun opcode ->
         List.mem could_be_these_opcodes opcode ~equal:[%compare.equal: Insn.Opcode.t]));
  while
    Array.exists opint_to_opcode ~f:(function
      | [] | [ _ ] -> false
      | _ :: _ :: _ -> true)
  do
    let one_candidate =
      Array.to_list opint_to_opcode
      |> List.filter_map ~f:(function
        | [ x ] -> Some x
        | _ -> None)
    in
    Array.map_inplace opint_to_opcode ~f:(function
      | [ _ ] as one -> one
      | _ as list ->
        List.filter
          list
          ~f:(Fn.non (List.mem one_candidate ~equal:[%compare.equal: Insn.Opcode.t])))
  done;
  let opint_to_opcode = Array.map opint_to_opcode ~f:List.hd_exn in
  let memory = [| 0; 0; 0; 0 |] in
  let%bind _, program = samples_and_program () in
  List.iter program ~f:(fun insn ->
    Insn.exec insn ~memory ~op:(Array.get opint_to_opcode));
  printf "%d\n" memory.(0);
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 573 |}];
  return ()
;;
