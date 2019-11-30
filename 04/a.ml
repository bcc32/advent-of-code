open! Core
open! Async
open! Import

let main () =
  let%bind events =
    Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list)
    (* times can be compared lexicographically *)
    >>| List.sort ~compare:String.compare
    >>| List.map ~f:Event.of_string
  in
  let sleeps = events |> Event.analyze |> Int.Table.of_alist_multi in
  let sleepiest_guard =
    sleeps
    |> Hashtbl.map ~f:(fun sleeps ->
      List.sum (module Int) sleeps ~f:(fun (x, y) -> y - x))
    |> Hashtbl.to_alist
    |> List.max_elt ~compare:(Comparable.lift [%compare: int] ~f:snd)
    |> Option.value_exn ~here:[%here]
    |> fst
  in
  let minute =
    let minutes_slept = Hashtbl.find_exn sleeps sleepiest_guard in
    List.init 60 ~f:(fun m ->
      m, List.count minutes_slept ~f:(fun (x, y) -> x <= m && m < y))
    |> List.max_elt ~compare:(Comparable.lift [%compare: int] ~f:snd)
    |> Option.value_exn ~here:[%here]
    |> fst
  in
  printf "%d\n" (sleepiest_guard * minute);
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 102688 |}]
;;
