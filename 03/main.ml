open! Core
open! Async
open! Import

module Point = struct
  type t = int * int [@@deriving compare, hash, sexp_of]
end

module Step = struct
  type t =
    | R
    | U
    | L
    | D
  [@@deriving sexp]

  let add (x, y) = function
    | R -> x + 1, y
    | L -> x - 1, y
    | U -> x, y + 1
    | D -> x, y - 1
  ;;
end

let follow path =
  let points = Hash_set.create (module Point) () in
  let p = ref (0, 0) in
  path
  |> List.iter ~f:(fun (n, step) ->
    for _ = 1 to n do
      p := Step.add !p step;
      Hash_set.add points !p
    done);
  points
;;

let parse_path_part part =
  let step = [%of_sexp: Step.t] (Sexp.Atom (String.make 1 part.[0])) in
  let n = Int.of_string (String.sub part ~pos:1 ~len:(String.length part - 1)) in
  n, step
;;

let a () =
  let%bind lines = Reader.file_lines "input" in
  let first =
    List.nth_exn lines 0 |> String.split ~on:',' |> List.map ~f:parse_path_part
  in
  let second =
    List.nth_exn lines 1 |> String.split ~on:',' |> List.map ~f:parse_path_part
  in
  let first_points = follow first in
  let second_points = follow second in
  let x, y =
    Hash_set.inter first_points second_points
    |> Hash_set.min_elt
         ~compare:
           (Comparable.lift [%compare: int] ~f:(fun (x, y) -> Int.abs x + Int.abs y))
    |> Option.value_exn
  in
  printf "%d\n" (Int.abs x + Int.abs y);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2050 |}]
;;

let follow path =
  let points = Hashtbl.create (module Point) in
  let p = ref (0, 0) in
  let c = ref 0 in
  path
  |> List.iter ~f:(fun (n, step) ->
    for _ = 1 to n do
      incr c;
      p := Step.add !p step;
      ignore (Hashtbl.add points ~key:!p ~data:!c : [ `Duplicate | `Ok ])
    done);
  points
;;

let b () =
  let%bind lines = Reader.file_lines "input" in
  let first =
    List.nth_exn lines 0 |> String.split ~on:',' |> List.map ~f:parse_path_part
  in
  let second =
    List.nth_exn lines 1 |> String.split ~on:',' |> List.map ~f:parse_path_part
  in
  let first_points = follow first in
  let second_points = follow second in
  let n =
    Hashtbl.merge first_points second_points ~f:(fun ~key:_ ->
      function
      | `Both (c1, c2) -> Some (c1 + c2)
      | `Left _ | `Right _ -> None)
    |> Hashtbl.data
    |> List.min_elt ~compare:[%compare: int]
    |> Option.value_exn
  in
  printf "%d\n" n;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 21666 |}]
;;
