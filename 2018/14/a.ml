open! Core
open! Async
open! Import

let main () =
  let%bind input = Reader.file_contents "input" >>| Int.of_string in
  let cocoa = Cocoa.create () in
  while Cocoa.length cocoa < input + 10 do
    Cocoa.make_more cocoa
  done;
  Cocoa.sub cocoa ~pos:input ~len:10
  |> Array.to_list
  |> List.map ~f:Int.to_string
  |> String.concat
  |> print_endline;
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 7861362411 |}]
;;
