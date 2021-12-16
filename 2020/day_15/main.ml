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

module Hashtbl_with_small_array = struct
  type 'a t =
    { array : 'a Option_array.t
    ; hashtbl : (int, 'a) Hashtbl.t
    }
  [@@deriving sexp_of]

  let create () =
    let array = Option_array.create ~len:3_000_000 in
    let hashtbl = Hashtbl.create (module Int) in
    { array; hashtbl }
  ;;

  let find t i =
    if i < Option_array.length t.array
    then Option_array.get t.array i
    else Hashtbl.find t.hashtbl i
  ;;

  let set t ~key:i ~data:x =
    if i < Option_array.length t.array
    then Option_array.set_some t.array i x
    else Hashtbl.set t.hashtbl ~key:i ~data:x
  ;;
end

let play numbers : int Iter.t =
  fun f ->
  let most_recent_index = Hashtbl_with_small_array.create () in
  let most_recent_index_of_last_number = ref None in
  List.iteri numbers ~f:(fun i number ->
    f number;
    most_recent_index_of_last_number
    := Hashtbl_with_small_array.find most_recent_index number;
    Hashtbl_with_small_array.set most_recent_index ~key:number ~data:i);
  let i = ref (List.length numbers) in
  while true do
    let yield n =
      f n;
      most_recent_index_of_last_number
      := Hashtbl_with_small_array.find most_recent_index n;
      Hashtbl_with_small_array.set most_recent_index ~key:n ~data:!i;
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
