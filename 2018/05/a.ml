open! Core
open! Async
open! Import

let main () =
  match%bind Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list) with
  | [ polymer ] ->
    let polymer = Polymer.react polymer in
    printf "%d\n" (String.length polymer);
    return ()
  | _ -> failwith "invalid input"
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 11194 |}];
  return ()
;;
