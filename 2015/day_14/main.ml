open! Core
open! Async
open! Import

module Reindeer = struct
  type t =
    { name : string
    ; speed : int
    ; time_flying : int
    ; time_resting : int
    }
  [@@deriving sexp_of]

  let parse_line =
    let parser =
      let open Angstrom in
      let name = take_while1 Char.is_alpha in
      let num = take_while1 Char.is_digit >>| Int.of_string in
      let* name = name in
      let* _ = string " can fly " in
      let* speed = num in
      let* _ = string " km/s for " in
      let* time_flying = num in
      let* _ = string " seconds, but then must rest for " in
      let* time_resting = num in
      let* _ = string " seconds." in
      return { name; speed; time_flying; time_resting }
    in
    fun line ->
      Angstrom.parse_string parser line ~consume:All
      |> Result.map_error ~f:Error.of_string
      |> ok_exn
  ;;
end

module State = struct
  type t =
    { reindeer : Reindeer.t
    ; mutable distance : int
    ; mutable resting : bool
    ; mutable time_left : int
    ; mutable points : int
    }
  [@@deriving sexp_of]

  let create reindeer =
    { reindeer
    ; distance = 0
    ; resting = false
    ; time_left = reindeer.time_flying
    ; points = 0
    }
  ;;

  let tick t =
    match t.resting with
    | true ->
      t.time_left <- t.time_left - 1;
      if t.time_left = 0
      then (
        t.resting <- false;
        t.time_left <- t.reindeer.time_flying)
    | false ->
      t.time_left <- t.time_left - 1;
      t.distance <- t.distance + t.reindeer.speed;
      if t.time_left = 0
      then (
        t.resting <- true;
        t.time_left <- t.reindeer.time_resting)
  ;;
end

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| (String.split_lines >> List.map ~f:Reindeer.parse_line))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input >>| List.map ~f:State.create in
  for _ = 1 to 2503 do
    List.iter input ~f:State.tick
  done;
  let ans =
    List.map input ~f:(fun state -> state.distance)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 2640 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input >>| List.map ~f:State.create in
  for _ = 1 to 2503 do
    List.iter input ~f:State.tick;
    let max_dist =
      List.map input ~f:(fun state -> state.distance)
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    List.iter input ~f:(fun state ->
      if state.distance = max_dist then state.points <- state.points + 1)
  done;
  let ans =
    List.map input ~f:(fun state -> state.points)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 1102 |}] in
  return ()
;;
