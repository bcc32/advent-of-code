open! Core
open! Async
open! Import

module Input = struct
  open! Advent_of_code_input_helpers

  type t =
    { earliest_timestamp : int
    ; bus_ids : int option list
    }
  [@@deriving sexp_of]

  let parse input : t =
    let lines = input |> lines in
    let earliest_timestamp = List.nth_exn lines 0 |> stoi in
    let bus_ids =
      List.nth_exn lines 1
      |> words ~sep:","
      |> List.map ~f:(function
        | "x" -> None
        | n -> Some (stoi n))
    in
    { earliest_timestamp; bus_ids }
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let find_first_bus earliest_timestamp bus_ids =
  List.map bus_ids ~f:(fun id ->
    let wait = -earliest_timestamp % id in
    id, wait)
  |> List.min_elt ~compare:(Comparable.lift Int.compare ~f:snd)
  |> uw
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let id, wait =
    find_first_bus input.earliest_timestamp (List.filter_opt input.bus_ids)
  in
  print_s [%sexp (id * wait : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 333 |}] in
  return ()
;;

let bezout a b =
  let open Bigint.O in
  let one = Bigint.of_int 1 in
  let rec loop r0 s0 t0 r1 s1 t1 =
    if r1 = zero
    then s0, t0, r0
    else (
      let q = r0 / r1 in
      loop r1 s1 t1 (r0 - (q * r1)) (s0 - (q * s1)) (t0 - (q * t1)))
  in
  loop a one zero b zero one
;;

let chinese_remainder_theorem residues =
  let open Bigint.O in
  let one = Bigint.of_int 1 in
  List.reduce_balanced_exn residues ~f:(fun (r1, m1) (r2, m2) ->
    let s, t, g = bezout m1 m2 in
    if g <> one
    then raise_s [%message "moduli not coprime" (m1 : Bigint.t) (m2 : Bigint.t)];
    let x = (r1 * t * m2) + (r2 * s * m1) in
    x, m1 * m2)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let id_at_time = input.bus_ids |> Array.of_list in
  let residues =
    Array.filter_mapi id_at_time ~f:(fun t id ->
      match id with
      | None -> None
      | Some id ->
        let required_residue = -t in
        Some (Bigint.of_int required_residue, Bigint.of_int id))
    |> Array.to_list
  in
  let x, y = chinese_remainder_theorem residues in
  let open Bigint.O in
  print_s [%sexp (x : Bigint.t), (y : Bigint.t), (x % y : Bigint.t)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| (110600785009168536379288168327779714849 878977779203641 690123192779524) |}] in
  return ()
;;
