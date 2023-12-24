open! Core
open! Async
open! Import

module Register = struct
  type t =
    | A
    | B
  [@@deriving bin_io, compare, enumerate, equal, sexp]

  module Total_map = Total_map.Make (struct
      type nonrec t = t [@@deriving bin_io, compare, enumerate, sexp]
    end)

  let parser =
    let open Angstrom in
    string "a" *> return A <|> string "b" *> return B
  ;;
end

module Instruction = struct
  type t =
    | Half of { register : Register.t }
    | Triple of { register : Register.t }
    | Increment of { register : Register.t }
    | Jump of { offset : int }
    | Jump_if_even of
        { register : Register.t
        ; offset : int
        }
    | Jump_if_one of
        { register : Register.t
        ; offset : int
        }
  [@@deriving sexp_of]

  let parser =
    let open Angstrom in
    let register = Register.parser in
    let int = take_while1 Char.is_digit >>| Int.of_string in
    let offset = char '-' *> int >>| Int.neg <|> char '+' *> int in
    let hlf =
      string "hlf "
      *> let+ register = register in
         Half { register }
    in
    let tpl =
      string "tpl "
      *> let+ register = register in
         Triple { register }
    in
    let inc =
      string "inc "
      *> let+ register = register in
         Increment { register }
    in
    let jmp =
      string "jmp "
      *> let+ offset = offset in
         Jump { offset }
    in
    let jie =
      string "jie "
      *> let* register = register in
         string ", "
         *> let+ offset = offset in
            Jump_if_even { register; offset }
    in
    let jio =
      string "jio "
      *> let* register = register in
         string ", "
         *> let+ offset = offset in
            Jump_if_one { register; offset }
    in
    choice [ hlf; tpl; inc; jmp; jie; jio ]
  ;;

  let of_string string =
    Angstrom.parse_string ~consume:All parser string
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
  ;;
end

module Program = struct
  type t = Instruction.t array [@@deriving sexp_of]

  let of_string = String.split_lines >> Array.of_list_map ~f:Instruction.of_string

  let step (t : t) ~heap ~index =
    match t.(index) with
    | Half { register } ->
      let heap = Total_map.change heap register ~f:(fun x -> x / 2) in
      heap, index + 1
    | Triple { register } ->
      let heap = Total_map.change heap register ~f:(fun x -> x * 3) in
      heap, index + 1
    | Increment { register } ->
      let heap = Total_map.change heap register ~f:Int.succ in
      heap, index + 1
    | Jump { offset } -> heap, index + offset
    | Jump_if_even { register; offset } ->
      if Total_map.find heap register % 2 = 0
      then heap, index + offset
      else heap, index + 1
    | Jump_if_one { register; offset } ->
      if Total_map.find heap register = 1 then heap, index + offset else heap, index + 1
  ;;

  let run t ~heap =
    let rec loop t ~heap ~index =
      if 0 <= index && index < Array.length t
      then (
        let heap, index = step t ~heap ~index in
        loop t ~heap ~index)
      else heap
    in
    loop t ~heap ~index:0
  ;;
end

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "aoc.in" >>| Program.of_string)
;;

let a () =
  let%bind program = Lazy_deferred.force_exn input in
  let heap = Register.Total_map.create_const 0 in
  let heap = Program.run program ~heap in
  let ans = Total_map.find heap B in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 255 |}];
  return ()
;;

let b () =
  let%bind program = Lazy_deferred.force_exn input in
  let heap =
    Register.Total_map.create (function
      | A -> 1
      | B -> 0)
  in
  let heap = Program.run program ~heap in
  let ans = Total_map.find heap B in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 334 |}];
  return ()
;;
