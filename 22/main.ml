open! Core
open! Async
open! Import

let debug = false

module Card = Int

module Technique = struct
  type t =
    | Deal_with_increment of int
    | Cut of int
    | Deal_into_new_stack

  let of_string s =
    match s with
    | "deal into new stack" -> Deal_into_new_stack
    | s ->
      (match String.chop_prefix s ~prefix:"cut " with
       | Some x -> Cut (Int.of_string x)
       | None ->
         (match String.chop_prefix s ~prefix:"deal with increment " with
          | Some x -> Deal_with_increment (Int.of_string x)
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

let input () =
  let%map lines = Reader.file_lines "input" in
  lines |> List.map ~f:Technique.of_string
;;

let a () =
  let%bind input = input () in
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

let modular_inverse n ~m = Z.invert (Z.of_int n) (Z.of_int m) |> Z.to_int

let mod_mul m a =
  let m = Bigint.of_int m in
  let a = Bigint.of_int a in
  fun z ->
    let open Bigint.O in
    a * Bigint.of_int z % m |> Bigint.to_int_exn
;;

(* let mod_mul_for =
 *   Memo.general (fun n -> mod_mul card_count (modular_inverse n ~card_count))
 * ;;
 *
 * (\* let mod_mul_for n z = mod_mul card_count (modular_inverse n) z *\)
 *
 * let where_did_it_come_from instruction card_index =
 *   match (instruction : Technique.t) with
 *   | Deal_with_increment n -> mod_mul_for n card_index
 *   | Deal_into_new_stack -> card_count - card_index - 1
 *   | Cut n -> (card_index + n) % card_count
 * ;;
 *
 * let where_did_it_come_from instructions card_index =
 *   Array.fold_right instructions ~init:card_index ~f:(fun insn index ->
 *     where_did_it_come_from insn index)
 * ;; *)

(* let step instructions = stage (fun index -> where_did_it_come_from instructions index)
 * let _ = step *)

(* let%expect_test _ =
 *   let%bind instructions = input () in
 *   Ref.set_temporarily card_count 10 ~f:(fun () ->
 *     let f = unstage (step instructions) in
 *     for i = 0 to 9 do
 *       printf "%d\n" (f i)
 *     done);
 *   [%expect {|
 *     9
 *     2
 *     5
 *     8
 *     1
 *     4
 *     7
 *     0
 *     3
 *     6 |}]
 * ;; *)

(* where_did_it_come_from for a single instruction is a linear mapping, and
   therefore so is where_did_it_come_from for multiple instructions.

   x -> a x + b

   Applying the above mapping N times gives:

   x
   a x + b
   a (a x + b) + b = a^2 x + ab + b
   a (a (a x + b) + b) + b = a^3 x + a^2 b + ab + b
   ...

   a^n x + b (1 - a^n) / (1 - a) *)

let transform_instruction ~card_count instruction (a, b) =
  match (instruction : Technique.t) with
  | Deal_with_increment n ->
    let minv = modular_inverse n ~m:card_count in
    mod_mul card_count a minv, mod_mul card_count b minv
  | Cut n -> a, (b + n) % card_count
  | Deal_into_new_stack -> -a, -b - 1
;;

let transform_all ~card_count instructions (a, b) =
  Array.fold_right instructions ~init:(a, b) ~f:(fun insn (a, b) ->
    transform_instruction ~card_count insn (a, b))
;;

let mod_exp m a b =
  assert (m > 0);
  Z.powm (Z.of_int a) (Z.of_int b) (Z.of_int m) |> Z.to_int
;;

let apply_n_times ~m ~n (a, b) =
  let a'n = mod_exp m a n in
  a'n, mod_mul m (mod_mul m b ((a'n - 1) % m)) (modular_inverse (a - 1) ~m)
;;

let%expect_test "apply_n_times" =
  Base_quickcheck.Test.run_exn
    (module struct
      type t = int * (int * int) [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (n, (a, b)) ->
      let m = 10_007 in
      if n >= 0 && n < 100
      then (
        let a = a % m in
        let b = b % m in
        match modular_inverse (a - 1) ~m with
        | exception _ -> ()
        | _ ->
          require_equal
            ~if_false_then_print_s:
              (lazy [%message (m : int) (n : int) (a : int) (b : int)])
            [%here]
            (module struct
              type t = int * int [@@deriving equal, sexp_of]
            end)
            (apply_n_times ~m ~n (a, b))
            (Fn.apply_n_times
               ~n
               (fun (a', b') -> mod_mul m a a', (mod_mul m a b' + b) % m)
               (1, 0))));
  [%expect {||}]
;;

let b () =
  let%bind instructions = input () >>| Array.of_list in
  let index = 2020 in
  let card_count = 119315717514047 in
  let shuffle_count = 101741582076661 in
  let a, b = transform_all instructions ~card_count (1, 0) in
  if debug then print_s [%message (a : int) (b : int)];
  let a, b = apply_n_times ~m:card_count ~n:shuffle_count (a, b) in
  if debug then print_s [%message (a : int) (b : int)];
  let index = ((a * index) + b) % card_count in
  printf "%d\n" index;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 452290953297 |}]
;;

let b' () =
  let%bind instructions = input () >>| Array.of_list in
  let index = 4684 in
  let card_count = 10_007 in
  let shuffle_count = 1 in
  let a, b =
    apply_n_times
      ~m:card_count
      ~n:shuffle_count
      (transform_all instructions ~card_count (1, 0))
  in
  let index = ((a * index) + b) % card_count in
  printf "%d\n" index;
  return ()
;;

let%expect_test "b'" =
  let%bind () = b' () in
  [%expect {| 2019 |}]
;;
