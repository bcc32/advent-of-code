open! Core
open! Async
open! Import

module Input = struct
  type t = int list [@@deriving sexp_of]

  let parse input : t = input |> String.split_lines |> List.map ~f:Int.of_string

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let order = 0 :: List.sort input ~compare:Int.compare in
  List.fold2_exn
    (List.drop_last_exn order)
    (List.tl_exn order)
    ~init:(0, 1) (* for last adapter to device *)
    ~f:(fun (one, three) prev next ->
      match next - prev with
      | 1 -> one + 1, three
      | 3 -> one, three + 1
      | 2 -> one, three
      | _ -> assert false)
  |> (fun (x, y) -> x * y)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 2100 |}] in
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
  let chain = List.sort input ~compare:Int.compare in
  count_ways ~chain ~start:0 ~end_:(List.last_exn chain + 3) |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 16198260678656 |}] in
  return ()
;;
