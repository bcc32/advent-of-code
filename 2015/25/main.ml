open! Core
open! Async
open! Import

let codes =
  Sequence.unfold_step ~init:20151125 ~f:(fun n ->
    Yield { value = n; state = n * 252533 % 33554393 })
;;

let places =
  Sequence.unfold_step ~init:(1, 1) ~f:(fun ((row, col) as cur) ->
    let next = if row = 1 then col + 1, 1 else row - 1, col + 1 in
    Yield { value = cur; state = next })
;;

let grid = Sequence.zip codes places

let parse =
  let pat =
    let open Re in
    compile (rep1 digit)
  in
  fun string ->
    match Re.matches pat string |> List.map ~f:Int.of_string with
    | [ row; col ] -> row, col
    | _ -> failwith "parse"
;;

let input = Lazy_deferred.create (fun () -> Reader.file_contents "aoc.in" >>| parse)

let a () =
  let%bind place = Lazy_deferred.force_exn input in
  let code =
    Sequence.find_map grid ~f:(fun (code, place') ->
      Option.some_if ([%equal: int * int] place place') code)
    |> Option.value_exn
  in
  print_s [%sexp (code : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 8997277 |}];
  return ()
;;
