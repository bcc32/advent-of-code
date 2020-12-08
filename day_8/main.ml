open! Core
open! Async
open! Import

module Instruction = struct
  type t =
    | Nop of int (* dummy offset *)
    | Acc of int
    | Jmp of int
  [@@deriving sexp_of]

  let op_re =
    let open Re in
    let opcode = alt [ str "nop"; str "acc"; str "jmp" ] in
    let offset = seq [ set "-+"; rep1 digit ] in
    compile (seq [ group opcode; str " "; group offset ])
  ;;

  let of_string line =
    let g = Re.exec op_re line in
    let offset = Re.Group.get g 2 |> Int.of_string in
    match Re.Group.get g 1 with
    | "nop" -> Nop offset
    | "acc" -> Acc offset
    | "jmp" -> Jmp offset
    | _ -> failwith "invalid opcode"
  ;;
end

module Input = struct
  type t = Instruction.t array [@@deriving sexp_of]

  let parse input : t =
    input |> String.split_lines |> List.map ~f:Instruction.of_string |> Array.of_list
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

module Run_result = struct
  type t =
    | Normal_end of int
    | Infinite_loop of int
  [@@deriving sexp_of]
end

let rec run (code : Instruction.t array) pc acc visited =
  if pc >= Array.length code
  then Run_result.Normal_end acc
  else (
    match Hash_set.strict_add visited pc with
    | Error _ -> Run_result.Infinite_loop acc
    | Ok () ->
      (match code.(pc) with
       | Nop _ -> run code (pc + 1) acc visited
       | Acc x -> run code (pc + 1) (acc + x) visited
       | Jmp x -> run code (pc + x) acc visited))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let visited = Hash_set.create (module Int) in
  let acc = run input 0 0 visited in
  print_s [%sexp (acc : Run_result.t)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| (Infinite_loop 1816) |}] in
  return ()
;;

let b () =
  let%bind code = Lazy_deferred.force_exn Input.t in
  let acc =
    with_return (fun { return } ->
      for i = 0 to Array.length code - 1 do
        match code.(i) with
        | Acc _ -> ()
        | Nop offset ->
          code.(i) <- Jmp offset;
          (match run code 0 0 (Hash_set.create (module Int)) with
           | Normal_end acc -> return acc
           | Infinite_loop _ -> ());
          code.(i) <- Nop offset
        | Jmp offset ->
          code.(i) <- Nop offset;
          (match run code 0 0 (Hash_set.create (module Int)) with
           | Normal_end acc -> return acc
           | Infinite_loop _ -> ());
          code.(i) <- Jmp offset
      done;
      assert false)
  in
  print_s [%sexp (acc : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 1149 |}] in
  return ()
;;
