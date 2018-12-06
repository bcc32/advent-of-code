open! Core
open! Async
open! Import

let main () =
  match%bind
    Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list)
  with
  | [ polymer ] ->
    let lower = Char.all |> List.filter ~f:Char.is_lowercase in
    lower
    |> List.map ~f:(fun unit ->
      let unit_removed =
        polymer |> String.filter ~f:(fun c -> Char.O.(Char.lowercase c <> unit))
      in
      String.length (Polymer.react unit_removed))
    |> List.min_elt ~compare:[%compare: int]
    |> Option.value_exn
    |> printf "%d\n";
    return ()
  | _ -> failwith "invalid input"
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 4178 |}]
;;
