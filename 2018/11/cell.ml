open! Core
open! Async
open! Import

type t = int * int

let grid_serial_number =
  Lazy_deferred.create (fun () ->
    let%map contents = Reader.file_contents "input" in
    Int.of_string contents)
;;

let power (x, y) =
  let rack_id = x + 10 in
  let power = rack_id * y in
  let%map grid_serial_number = Lazy_deferred.force_exn grid_serial_number in
  let power = power + grid_serial_number in
  let power = power * rack_id in
  let power = power / 100 % 10 in
  power - 5
;;

let all_powers () =
  Array.init 300 ~f:(fun x ->
    let x = x + 1 in
    Array.init 300 ~f:(fun y ->
      let y = y + 1 in
      power (x, y))
    |> Deferred.Array.all)
  |> Deferred.Array.all
;;
