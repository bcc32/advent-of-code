open! Core
open! Async
open! Import

module Input = struct
  type t = int list [@@deriving sexp_of]

  let parse input : t =
    input
    |> String.split_lines
    |> List.map ~f:Int.of_string
    |> List.sort ~compare:Int.compare
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "aoc.in" >>| parse)
  ;;
end

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let order = (0 :: input) @ [ List.last_exn input + 3 ] |> Array.of_list in
  let ones = ref 0 in
  let threes = ref 0 in
  for i = 0 to Array.length order - 2 do
    let prev = order.(i) in
    let next = order.(i + 1) in
    match next - prev with
    | 1 -> incr ones
    | 2 -> ()
    | 3 -> incr threes
    | n -> raise_s [%message "invalid diff" (n : int)]
  done;
  print_s [%sexp (!ones * !threes : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2100 |}];
  return ()
;;

module Memoized_input = struct
  type t =
    { start : int
    ; end_ : int
    ; chain : int list
    }
  [@@deriving compare, hash, sexp_of]
end

let cache = Hashtbl.create (module Memoized_input)

let rec count_ways ~start ~end_ ~chain =
  Hashtbl.findi_or_add
    cache
    ({ start; end_; chain } : Memoized_input.t)
    ~default:(fun { start; end_; chain } ->
      match chain with
      | [] -> if end_ - start <= 3 then 1 else 0
      | hd :: tl ->
        (if hd - start <= 3 then count_ways ~start:hd ~end_ ~chain:tl else 0)
        + count_ways ~start ~end_ ~chain:tl)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  count_ways ~chain:input ~start:0 ~end_:(3 + List.last_exn input)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 16198260678656 |}];
  return ()
;;
