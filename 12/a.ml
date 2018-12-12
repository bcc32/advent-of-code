open! Core
open! Async
open! Import

let main () =
  let%bind game = Game.read () in
  let state =
    let rec loop state n =
      if false then Debug.eprint_s [%message "loop" (state : State.t)];
      if n > 0 then loop (State.step state game.step) (n - 1) else state
    in
    loop (State.of_init game.init) 20
  in
  if false then Debug.eprint_s [%message "" (game : Game.t) (state : State.t)];
  let sum = ref 0 in
  State.iteri state ~f:(fun i b -> if b then sum := !sum + i);
  printf "%d\n" !sum;
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 3061 |}]
;;
