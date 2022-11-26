open! Core
open! Async
open! Import

let main () =
  let%bind carts = Carts.read () in
  with_return (fun { return } ->
    while true do
      Carts.tick carts ~on_collision:(fun ~row:_ ~col:_ -> ());
      match Carts.lone_ranger carts with
      | None -> ()
      | Some (row, col) ->
        printf "%d,%d\n" col row;
        return ()
    done);
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 50,21 |}];
  return ()
;;
