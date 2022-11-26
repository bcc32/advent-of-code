open! Core
open! Async
open! Import

module Input = struct
  type t = int list * int list [@@deriving sexp_of]

  let parse input : t =
    let inputs = input |> String.split_lines |> List.map ~f:Int.of_string in
    List.take inputs 25, List.drop inputs 25
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let can_sum preamble ~to_ =
  with_return (fun { return } ->
    Fqueue.iter preamble ~f:(fun x ->
      Fqueue.iter preamble ~f:(fun y -> if x <> y && x + y = to_ then return true));
    false)
;;

let find_first_not_possible preamble list =
  List.fold_until
    list
    ~init:(Fqueue.of_list preamble)
    ~f:(fun preamble x ->
      if can_sum preamble ~to_:x
      then Continue (Fqueue.enqueue (Fqueue.drop_exn preamble) x)
      else Stop x)
    ~finish:(fun _ -> assert false)
;;

let a () =
  let%bind preamble, list = Lazy_deferred.force_exn Input.t in
  let x = find_first_not_possible preamble list in
  print_s [%sexp (x : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 41682220 |}];
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
        if get_prefix_sum ~upto:j - get_prefix_sum ~upto:i = target then return (i, j - i)
      done
    done;
    assert false)
;;

let b () =
  let%bind preamble, list = Lazy_deferred.force_exn Input.t in
  let x = find_first_not_possible preamble list in
  let pos, len = find_contiguous_sum (preamble @ list) x in
  let sublist = List.sub (preamble @ list) ~pos ~len in
  assert (List.sum (module Int) sublist ~f:Fn.id = x);
  let min = List.min_elt sublist ~compare:Int.compare |> Option.value_exn in
  let max = List.max_elt sublist ~compare:Int.compare |> Option.value_exn in
  let ans = min + max in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 5388976 |}];
  return ()
;;
