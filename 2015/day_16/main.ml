open! Core
open! Async
open! Import

let requires =
  [ "children", 3
  ; "cats", 7
  ; "samoyeds", 2
  ; "pomeranians", 3
  ; "akitas", 0
  ; "vizslas", 0
  ; "goldfish", 5
  ; "trees", 3
  ; "cars", 2
  ; "perfumes", 1
  ]
;;

let parse_line =
  let parser =
    let open Angstrom in
    let thing =
      let* name = take_while1 Char.is_alpha in
      string ": "
      *> let* num =
           let+ digits = take_while1 Char.is_digit in
           Int.of_string digits
      in
      return (name, num)
    in
    string "Sue "
    *> let* index = take_while1 Char.is_digit >>| Int.of_string in
    string ": "
    *> let* things = sep_by1 (string ", ") thing in
    return (index, things)
  in
  fun line ->
    Angstrom.parse_string parser line ~consume:All
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| (String.split_lines >> List.map ~f:parse_line))
;;

let aunt_matches (_index, items) =
  List.for_all items ~f:(fun (item, count) ->
    List.Assoc.find_exn requires item ~equal:String.equal = count)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let aunt, _ = List.find_exn input ~f:aunt_matches in
  print_s [%sexp (aunt : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 103 |}];
  return ()
;;

let aunt_matches (_index, items) =
  List.for_all items ~f:(fun (item, count) ->
    let rq = List.Assoc.find_exn requires item ~equal:String.equal in
    match item with
    | "cats" | "trees" -> count > rq
    | "pomeranians" | "goldfish" -> count < rq
    | _ -> rq = count)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let aunt, _ = List.find_exn input ~f:aunt_matches in
  print_s [%sexp (aunt : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 405 |}];
  return ()
;;
