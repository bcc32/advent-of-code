open! Core
open! Async
open! Import

let use_example_input = false

module Dir = struct
  type t =
    | E
    | SE
    | SW
    | W
    | NW
    | NE
  [@@deriving enumerate, sexp]

  let of_string = String.uppercase >> Sexp.of_string >> [%of_sexp: t]
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Dir.t list list [@@deriving sexp_of]

  let parse input : t =
    let re =
      let open Re in
      compile (alt [ str "e"; str "se"; str "sw"; str "w"; str "nw"; str "ne" ])
    in
    input
    |> lines
    |> List.map ~f:(fun line ->
      Re.all re line
      |> List.map ~f:(Fn.flip Re.Group.get 0)
      |> List.map ~f:Dir.of_string)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () ->
      Reader.file_contents
        (if use_example_input then "input.example.txt" else "input.txt")
      >>| parse)
  ;;
end

module Coord = struct
  type t =
    { x : int
    ; ne : int
    }
  [@@deriving compare, equal, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  (* {v
      A B
     C D E
      F G
        v} *)
  let incr { x; ne } (dir : Dir.t) =
    match dir with
    | E -> { x = x + 1; ne }
    | SE -> { x = x + 1; ne = ne - 1 }
    | SW -> { x; ne = ne - 1 }
    | W -> { x = x - 1; ne }
    | NW -> { x = x - 1; ne = ne + 1 }
    | NE -> { x; ne = ne + 1 }
  ;;

  let origin = { x = 0; ne = 0 }
  let neighbors t = Dir.all |> List.map ~f:(fun dir -> incr t dir)
end

let find_point_from_origin (dirs : Dir.t list) =
  List.fold dirs ~init:Coord.origin ~f:(fun cur dir -> Coord.incr cur dir)
;;

let set_hash_set_membership hs x bool =
  if bool then Hash_set.add hs x else Hash_set.remove hs x
;;

let setup_board input black =
  List.iter input ~f:(fun dirs ->
    let point = find_point_from_origin dirs in
    set_hash_set_membership black point (not (Hash_set.mem black point)))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let black = Hash_set.create (module Coord) in
  setup_board input black;
  print_s [%sexp (Hash_set.length black : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 394 |}];
  return ()
;;

let do_flip black =
  let tiles_to_consider =
    Hash_set.to_list black
    |> List.concat_map ~f:(fun t -> t :: Coord.neighbors t)
    |> Set.stable_dedup_list (module Coord)
  in
  List.map tiles_to_consider ~f:(fun t ->
    let count_black_neighbors = List.count (Coord.neighbors t) ~f:(Hash_set.mem black) in
    let black_in_next_iter =
      match Hash_set.mem black t with
      | true -> not (count_black_neighbors = 0 || count_black_neighbors > 2)
      | false -> count_black_neighbors = 2
    in
    t, black_in_next_iter)
  |> List.iter ~f:(fun (t, black_in_next_iter) ->
    set_hash_set_membership black t black_in_next_iter)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let black = Hash_set.create (module Coord) in
  setup_board input black;
  for _ = 1 to 100 do
    do_flip black
  done;
  print_s [%sexp (Hash_set.length black : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 4036 |}];
  return ()
;;
