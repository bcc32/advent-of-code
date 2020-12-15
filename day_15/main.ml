open! Core
open! Async
open! Import

module Input = struct
  open! Advent_of_code_input_helpers

  type t = int list [@@deriving sexp_of]

  let parse input : t = input |> String.strip |> words ~sep:"," |> List.map ~f:stoi

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let play numbers : int Iter.t =
  fun f ->
  let most_recent_index = Hashtbl.create (module Int) in
  let most_recent_index_of_last_number = ref None in
  List.iteri numbers ~f:(fun i number ->
    f number;
    most_recent_index_of_last_number := Hashtbl.find most_recent_index number;
    Hashtbl.set most_recent_index ~key:number ~data:i);
  let i = ref (List.length numbers) in
  while true do
    let yield n =
      f n;
      most_recent_index_of_last_number := Hashtbl.find most_recent_index n;
      Hashtbl.set most_recent_index ~key:n ~data:!i;
      incr i
    in
    match !most_recent_index_of_last_number with
    | None -> yield 0
    | Some i' ->
      (* Subtract one because we're thinking about the previous index of the
         number at [i - 1]. *)
      yield (!i - i' - 1)
  done
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let ans = play input |> Iter.drop 2019 |> Iter.head_exn in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 1238 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let ans = play input |> Iter.drop 29999999 |> Iter.head_exn in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 3745954 |}] in
  return ()
;;
