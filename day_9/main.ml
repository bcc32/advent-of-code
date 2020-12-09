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

let can_sum list x =
  let list = Fqueue.to_list list in
  List.existsi list ~f:(fun i a ->
    List.sub list ~pos:(i + 1) ~len:(List.length list - (i + 1))
    |> List.exists ~f:(fun b -> a <> b && a + b = x))
;;

let rec find_first_not_possible preamble list =
  match list with
  | [] -> None
  | hd :: tl ->
    if can_sum preamble hd
    then
      find_first_not_possible (Fqueue.enqueue (snd (Fqueue.dequeue_exn preamble)) hd) tl
    else Some hd
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let x =
    find_first_not_possible
      (Fqueue.of_list (List.sub input ~pos:0 ~len:25))
      (List.drop input 25)
  in
  print_s [%sexp (x : int option)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| (41682220) |}] in
  return ()
;;

let prefix_sum list = List.folding_map list ~init:0 ~f:(fun acc x -> x + acc, x + acc)

let find_contiguous_sum list target =
  let prefix_sums = prefix_sum list |> Array.of_list in
  let get_prefix_sum ~upto:pos =
    match pos with
    | 0 -> 0
    | n -> prefix_sums.(n - 1)
  in
  with_return (fun { return } ->
    for i = 0 to Array.length prefix_sums - 1 do
      for j = i to Array.length prefix_sums - 1 do
        if get_prefix_sum ~upto:j - get_prefix_sum ~upto:i = target
        then return (i, j - i)
      done
    done;
    assert false)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let x =
    find_first_not_possible
      (Fqueue.of_list (List.sub input ~pos:0 ~len:25))
      (List.drop input 25)
    |> uw
  in
  let pos, len = find_contiguous_sum input x in
  let sublist = List.sub input ~pos ~len in
  assert (List.sum (module Int) sublist ~f:Fn.id = x);
  let min = List.min_elt sublist ~compare:Int.compare |> uw in
  let max = List.max_elt sublist ~compare:Int.compare |> uw in
  let ans = min + max in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 5388976 |}] in
  return ()
;;
