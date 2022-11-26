open! Core
open! Async
open! Import

let re = Re.(compile (seq [ group (rep1 digit); str ", "; group (rep1 digit) ]))
let dist (x, y) (x', y') = Int.abs (x - x') + Int.abs (y - y')
let max_dist = 10_000

let main () =
  let%bind points =
    Reader.with_file "input" ~f:(fun r ->
      r
      |> Reader.lines
      |> Pipe.map ~f:(fun line ->
        let group = Re.exec re line in
        let x = Re.Group.get group 1 |> Int.of_string in
        let y = Re.Group.get group 2 |> Int.of_string in
        x, y)
      |> Pipe.to_list)
  in
  let min_x, max_x, min_y, max_y =
    let x0, y0 = List.hd_exn points in
    List.fold points ~init:(x0, x0, y0, y0) ~f:(fun (min_x, max_x, min_y, max_y) (x, y) ->
      Int.min min_x x, Int.max max_x x, Int.min min_y y, Int.max max_y y)
  in
  let count = ref 0 in
  let dist_sum p = List.sum (module Int) points ~f:(dist p) in
  let margin = max_dist / List.length points in
  for x = min_x - margin to max_x + margin do
    for y = min_y - margin to max_y + margin do
      if dist_sum (x, y) < max_dist then incr count
    done
  done;
  printf "%d\n" !count;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 36216 |}];
  return ()
;;
