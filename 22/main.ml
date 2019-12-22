open! Core
open! Async
open! Import

let debug = false

module Card = Int

module Technique = struct
  type 'int t =
    | Deal_with_increment of 'int
    | Cut of 'int
    | Deal_into_new_stack

  let of_string s ~int_of_string =
    match s with
    | "deal into new stack" -> Deal_into_new_stack
    | s ->
      (match String.chop_prefix s ~prefix:"cut " with
       | Some x -> Cut (int_of_string x)
       | None ->
         (match String.chop_prefix s ~prefix:"deal with increment " with
          | Some x -> Deal_with_increment (int_of_string x)
          | None -> failwith "Technique.of_string"))
  ;;

  let perform t deck =
    match t with
    | Deal_into_new_stack -> Array.rev_inplace deck
    | Cut n ->
      let next = Array.create 0 ~len:(Array.length deck) in
      if n > 0
      then (
        Array.blit ~src:deck ~dst:next ~src_pos:n ~len:(Array.length deck - n) ~dst_pos:0;
        Array.blit ~src:deck ~dst:next ~src_pos:0 ~len:n ~dst_pos:(Array.length deck - n))
      else (
        let n = Int.abs n in
        Array.blit ~src:deck ~dst:next ~src_pos:0 ~len:(Array.length deck - n) ~dst_pos:n;
        Array.blit ~src:deck ~dst:next ~src_pos:(Array.length deck - n) ~len:n ~dst_pos:0);
      Array.blito ~src:next ~dst:deck ()
    | Deal_with_increment n ->
      let next = Array.create 0 ~len:(Array.length deck) in
      let into_index = ref 0 in
      Array.iter deck ~f:(fun x ->
        next.(!into_index) <- x;
        into_index := !into_index + n;
        into_index := !into_index % Array.length deck);
      Array.blito ~src:next ~dst:deck ()
  ;;
end

let input ~int_of_string =
  let%map lines = Reader.file_lines "input" in
  lines |> List.map ~f:(Technique.of_string ~int_of_string)
;;

let a () =
  let%bind input = input ~int_of_string:Int.of_string in
  let deck = Array.init 10_007 ~f:Fn.id in
  List.iter input ~f:(fun t -> Technique.perform t deck);
  if debug then print_s [%sexp (deck : int array)];
  let i, _ = Array.findi_exn deck ~f:(fun _ x -> x = 2019) in
  printf "%d\n" i;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 4684 |}]
;;

(* Each shuffle technique is a linear mapping of positions (modulo card_count).

   In the forward direction:

   - cut(n): (a, b) -> (a, b - n)
   - deal(n): (a, b) -> (an, bn)
   - deal new: (a, b) -> (-a, -b - 1)

   In the reverse direction:

   - cut(n): (a, b) -> (a, b + n)
   - deal(n): (a, b) -> (an^(-1), bn^(-1))
   - deal new: (a, b) -> (-a, -b - 1)

   Applying the transformation n times is equal to:

   a^n x + a^(n-1) b + a^(n-2) b + ... + b, which is just the sum of a geometric
   series:

   a^n x + b (1 - a^n) / (1 - a)
*)

open Bigint.O

let z = Bigint.to_zarith_bigint
let z' = Bigint.of_zarith_bigint
let modular_inverse n ~m = Z.invert (z n) (z m) |> z'

let transform_instruction ~card_count (instruction : Bigint.t Technique.t) (a, b) =
  match instruction with
  | Deal_with_increment n ->
    let minv = modular_inverse n ~m:card_count in
    a * minv % card_count, b * minv % card_count
  | Cut n -> a, (b + n) % card_count
  | Deal_into_new_stack -> -a, -b - Bigint.one
;;

let transform_all ~card_count instructions =
  List.fold_right instructions ~init:(Bigint.one, Bigint.zero) ~f:(fun insn (a, b) ->
    transform_instruction ~card_count insn (a, b))
;;

let mod_exp ~m a b =
  (* Zarith has a bug (fixed in master) where the program crashes with SIGFPE if
     the modulus is zero. *)
  assert (Bigint.is_positive m);
  Z.powm (z a) (z b) (z m) |> z'
;;

let apply_n_times ~m ~n (a, b) =
  let a'n = mod_exp ~m a n in
  a'n, b * (Bigint.one - a'n) * modular_inverse (Bigint.one - a) ~m % m
;;

let b () =
  let%bind instructions = input ~int_of_string:Bigint.of_string in
  let index = Bigint.of_int 2020 in
  let card_count = Bigint.of_int 119315717514047 in
  let shuffle_count = Bigint.of_int 101741582076661 in
  let a, b = transform_all instructions ~card_count in
  if debug then print_s [%message (a : Bigint.t) (b : Bigint.t)];
  let a, b = apply_n_times ~m:card_count ~n:shuffle_count (a, b) in
  if debug then print_s [%message (a : Bigint.t) (b : Bigint.t)];
  let index = ((a * index) + b) % card_count in
  printf !"%{Bigint}\n" index;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 452290953297 |}]
;;

let b' () =
  let%bind instructions = input ~int_of_string:Bigint.of_string in
  let index = Bigint.of_int 4684 in
  let card_count = Bigint.of_int 10_007 in
  let shuffle_count = Bigint.of_int 1 in
  let a, b =
    apply_n_times ~m:card_count ~n:shuffle_count (transform_all instructions ~card_count)
  in
  let index = ((a * index) + b) % card_count in
  printf !"%{Bigint}\n" index;
  return ()
;;

let%expect_test "b'" =
  let%bind () = b' () in
  [%expect {| 2019 |}]
;;
