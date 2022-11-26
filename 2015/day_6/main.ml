open! Core
open! Async
open! Import

type inst =
  | Off
  | On
  | Toggle

type coord = int * int
type action = inst * coord * coord

let regexp =
  let open Re in
  compile
    (seq
       [ group (alt [ str "turn off"; str "turn on"; str "toggle" ])
       ; str " "
       ; group (rep1 digit)
       ; char ','
       ; group (rep1 digit)
       ; str " through "
       ; group (rep1 digit)
       ; char ','
       ; group (rep1 digit)
       ])
;;

let action_of_line line =
  let group = Re.exec regexp line in
  match Re.Group.all group with
  | [| _; inst; x1; y1; x2; y2 |] ->
    let inst =
      match inst with
      | "turn off" -> Off
      | "turn on" -> On
      | "toggle" -> Toggle
      | _ -> failwith "bad inst"
    in
    let x1 = Int.of_string x1 in
    let y1 = Int.of_string y1 in
    let x2 = Int.of_string x2 in
    let y2 = Int.of_string y2 in
    inst, (x1, y1), (x2, y2)
  | _ -> raise_s [%message "invalid line" (line : string)]
;;

let input : action list Lazy_deferred.t =
  Lazy_deferred.create (fun () ->
    Reader.file_lines "input.txt" >>| List.map ~f:action_of_line)
;;

type state = bool array array

let init () : state = Array.init 1_000 ~f:(fun _ -> Array.init 1_000 ~f:(Fn.const false))

let perform state (inst, (x1, y1), (x2, y2)) =
  for x = x1 to x2 do
    for y = y1 to y2 do
      match inst with
      | On -> state.(x).(y) <- true
      | Off -> state.(x).(y) <- false
      | Toggle -> state.(x).(y) <- not state.(x).(y)
    done
  done
;;

let lights_on state = Array.sum (module Int) state ~f:(Array.count ~f:Fn.id)

let a () =
  let%bind actions = Lazy_deferred.force_exn input in
  let state = init () in
  List.iter actions ~f:(perform state);
  let lights = lights_on state in
  print_s [%sexp (lights : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 400410 |}];
  return ()
;;

let init () = Array.init 1_000 ~f:(fun _ -> Array.init 1_000 ~f:(Fn.const 0))

let perform state (inst, (x1, y1), (x2, y2)) =
  for x = x1 to x2 do
    for y = y1 to y2 do
      match inst with
      | On -> state.(x).(y) <- state.(x).(y) + 1
      | Off -> state.(x).(y) <- Int.max 0 (state.(x).(y) - 1)
      | Toggle -> state.(x).(y) <- state.(x).(y) + 2
    done
  done
;;

let lights_on state = Array.sum (module Int) state ~f:(Array.sum (module Int) ~f:Fn.id)

let b () =
  let%bind actions = Lazy_deferred.force_exn input in
  let state = init () in
  List.iter actions ~f:(perform state);
  let lights = lights_on state in
  print_s [%sexp (lights : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 15343601 |}];
  return ()
;;
