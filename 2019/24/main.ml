open! Core
open! Async
open! Import

module Bitmap = struct
  (** A bitmap is an int whose bits represent the tiles of the map, with an
      extra bit around the border of each side.

      We use 49 of the bits of the integer, like so:

      {v
        00   01 02 03 04 05   06
           +----------------+
        07 | 08 09 10 11 12 | 13
        14 | 15 16 17 18 19 | 20
        21 | 22 23 24 25 26 | 27
        28 | 29 30 31 32 33 | 34
        35 | 36 37 38 39 40 | 41
           +----------------+
        42   43 44 45 46 47   48
       v}

      Bits outside the border are always treated as zero, enforced by
      {!border_mask}.

      Bits are indexed from 0 starting with the LSB. *)
  type t = Int63.t [@@deriving equal, sexp_of]

  let border_mask =
    Int63.of_int 0b0_00000_0__0_11111_00_11111_00_11111_00_11111_00_11111_0__0_00000_0
  ;;

  (* We should only have as many bits set as there are tiles in the map. *)
  let%test _ = Int63.popcount border_mask = 25

  let to_grid t =
    Array.init 5 ~f:(fun i ->
      let r0 = (7 * i) + 8 in
      Array.init 5 ~f:(fun j ->
        let bit_index = r0 + j in
        let open Int63.O in
        (t lsr bit_index) land Int63.one <> zero))
  ;;

  let to_string_hum t =
    t
    |> to_grid
    |> Array.map ~f:(fun row ->
      row
      |> Array.map ~f:(function
        | false -> "."
        | true -> "#")
      |> String.concat_array)
    |> String.concat_array ~sep:"\n"
  ;;

  let of_grid grid =
    Array.foldi grid ~init:Int63.zero ~f:(fun i t row ->
      let r0 = (7 * i) + 8 in
      Array.foldi row ~init:t ~f:(fun j t bool ->
        let bit_index = r0 + j in
        let open Int63.O in
        if bool then t lor (Int63.one lsl bit_index) else t))
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let open Int63.O in
      assert (t land lnot border_mask = zero))
  ;;

  let step t =
    let open Int63.O in
    let up = t lsl 7 in
    let left = t lsl 1 in
    let right = t lsr 1 in
    let down = t lsr 7 in
    let exactly_one_bug_adjacent =
      up
      land lnot left
      land lnot right
      land lnot down
      lor (lnot up land left land lnot right land lnot down)
      lor (lnot up land lnot left land right land lnot down)
      lor (lnot up land lnot left land lnot right land down)
    in
    let exactly_two_bugs_adjacent =
      up
      land left
      land lnot right
      land lnot down
      lor (up land lnot left land right land lnot down)
      lor (up land lnot left land lnot right land down)
      lor (lnot up land left land right land lnot down)
      lor (lnot up land left land lnot right land down)
      lor (lnot up land lnot left land right land down)
    in
    let bug_survives = t land exactly_one_bug_adjacent in
    let bug_is_born =
      lnot t land (exactly_one_bug_adjacent lor exactly_two_bugs_adjacent)
    in
    let t = bug_survives lor bug_is_born land border_mask in
    invariant t;
    t
  ;;

  let rating t =
    t
    |> to_grid
    |> Array.foldi ~init:0 ~f:(fun i accum row ->
      Array.foldi row ~init:accum ~f:(fun j accum bool ->
        let index = (5 * i) + j in
        if bool then accum + (1 lsl index) else accum))
  ;;
end

let input () =
  let%map lines = Reader.file_lines "input" in
  lines
  |> Array.of_list_map ~f:(fun line ->
    line
    |> String.to_array
    |> Array.map ~f:(function
      | '.' | '?' -> false
      | '#' -> true
      | c -> failwithf "unrecognized char: %c" c ()))
;;

(* FIXME: Deduplicate from problem 12. *)
let find_cycle_length init ~equal ~step =
  let rec loop power cycle_length slow fast =
    if equal slow fast
    then cycle_length
    else if power = cycle_length
    then loop (power * 2) 1 fast (step fast)
    else loop power (cycle_length + 1) slow (step fast)
  in
  let cycle_length = loop 1 1 init (step init) in
  let rec loop offset slow fast =
    if equal slow fast then offset else loop (offset + 1) (step slow) (step fast)
  in
  let offset = loop 0 init (Fn.apply_n_times step init ~n:cycle_length) in
  offset, cycle_length
;;

let%expect_test "step" =
  let%bind bitmap = input () >>| Bitmap.of_grid in
  printf !"%{Bitmap#hum}\n" bitmap;
  let%bind () = [%expect {|
    ..##.
    #....
    .....
    #.#.#
    #..#. |}] in
  let bitmap = Bitmap.step bitmap in
  printf !"%{Bitmap#hum}\n" bitmap;
  let%bind () = [%expect {|
    #####
    .###.
    #.#.#
    ##...
    ###.# |}] in
  let bitmap = Bitmap.step bitmap in
  printf !"%{Bitmap#hum}\n" bitmap;
  let%bind () = [%expect {|
    #...#
    .....
    #.#..
    ....#
    ..##. |}] in
  let bitmap = Bitmap.step bitmap in
  printf !"%{Bitmap#hum}\n" bitmap;
  let%bind () = [%expect {|
    .#.#.
    #.#.#
    .#.##
    #.##.
    .#### |}] in
  let bitmap = Bitmap.step bitmap in
  printf !"%{Bitmap#hum}\n" bitmap;
  let%bind () = [%expect {|
    #...#
    ....#
    .....
    .....
    ##..# |}] in
  return ()
;;

let a () =
  let%bind bitmap = input () >>| Bitmap.of_grid in
  let offset, cycle_length =
    find_cycle_length bitmap ~equal:[%equal: Bitmap.t] ~step:Bitmap.step
  in
  print_s [%message (offset : int) (cycle_length : int)];
  let first_repeated = Fn.apply_n_times ~n:offset Bitmap.step bitmap in
  printf !"%{Bitmap#hum}\n" first_repeated;
  printf "%d\n" (Bitmap.rating first_repeated);
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect
    {|
    ((offset       4)
     (cycle_length 58))
    #...#
    ....#
    .....
    .....
    ##..#
    19923473 |}]
;;

module Position = struct
  module Within_level = struct
    (** A position within a level.

        {v
          ABCDE
          FGHIJ
          KLMNO
          PQRST
          UVWXY
         v} *)
    module T = struct
      type t =
        | A
        | B
        | C
        | D
        | E
        | F
        | G
        | H
        | I
        | J
        | K
        | L
        | M
        | N
        | O
        | P
        | Q
        | R
        | S
        | T
        | U
        | V
        | W
        | X
        | Y
      [@@deriving compare, enumerate, sexp_of]

      include (val Comparator.make ~compare ~sexp_of_t)
    end

    include T

    let of_int_exn =
      Map.find_exn
        (List.mapi all ~f:(fun i pos -> i, pos) |> Map.of_alist_exn (module Int))
    ;;

    let to_int =
      Map.find_exn (List.mapi all ~f:(fun i pos -> pos, i) |> Map.of_alist_exn (module T))
    ;;

    let down_exn t = of_int_exn (to_int t + 5)
    let left_exn t = of_int_exn (to_int t - 1)
    let right_exn t = of_int_exn (to_int t + 1)
    let up_exn t = of_int_exn (to_int t - 5)

    let adjacent_same_level = function
      | M -> invalid_arg "adjacent_same_level"
      | A -> [ B; F ]
      | (B | C | D) as t -> [ left_exn t; down_exn t; right_exn t ]
      | E -> [ D; J ]
      | (F | K | P) as t -> [ up_exn t; right_exn t; down_exn t ]
      | (G | I | Q | S) as t -> [ up_exn t; right_exn t; down_exn t; left_exn t ]
      | H -> [ C; G; I ]
      | (J | O | T) as t -> [ up_exn t; left_exn t; down_exn t ]
      | L -> [ G; K; Q ]
      | N -> [ I; O; S ]
      | R -> [ Q; S; W ]
      | U -> [ P; V ]
      | (V | W | X) as t -> [ left_exn t; up_exn t; right_exn t ]
      | Y -> [ X; T ]
    ;;

    let adjacent_inward_level = function
      | M -> invalid_arg "adjacent_inward_level"
      | A | B | C | D | E | F | G | I | J | K | O | P | Q | S | T | U | V | W | X | Y ->
        []
      | H -> [ A; B; C; D; E ]
      | L -> [ A; F; K; P; U ]
      | N -> [ E; J; O; T; Y ]
      | R -> [ U; V; W; X; Y ]
    ;;

    let adjacent_outward_level = function
      | M -> invalid_arg "adjacent_outward_level"
      | G | H | I | L | N | Q | R | S -> []
      | A -> [ L; H ]
      | B | C | D -> [ H ]
      | E -> [ H; N ]
      | F | K | P -> [ L ]
      | J | O | T -> [ N ]
      | U -> [ L; R ]
      | V | W | X -> [ R ]
      | Y -> [ N; R ]
    ;;
  end

  type t = int * Within_level.t [@@deriving compare, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  let adjacent (level, within_level) =
    (Within_level.adjacent_same_level within_level
     |> List.map ~f:(fun within_level -> level, within_level))
    @ (Within_level.adjacent_inward_level within_level
       |> List.map ~f:(fun within_level -> level + 1, within_level))
    @ (Within_level.adjacent_outward_level within_level
       |> List.map ~f:(fun within_level -> level - 1, within_level))
  ;;
end

let step bugs =
  Set.elements bugs
  |> List.concat_map ~f:Position.adjacent
  |> Set.of_list (module Position)
  |> Set.filter ~f:(fun pos ->
    let is_bug = Set.mem bugs pos in
    let neighbor_count = Position.adjacent pos |> List.count ~f:(Set.mem bugs) in
    if is_bug then neighbor_count = 1 else neighbor_count = 1 || neighbor_count = 2)
;;

let%expect_test "adjacent" =
  let test pos = print_s [%sexp (Position.adjacent pos : Position.t list)] in
  test (0, S);
  let%bind () = [%expect {|
    ((0 N)
     (0 T)
     (0 X)
     (0 R)) |}] in
  test (1, G);
  let%bind () = [%expect {|
    ((1 B)
     (1 H)
     (1 L)
     (1 F)) |}] in
  test (1, D);
  let%bind () = [%expect {|
    ((1 C)
     (1 I)
     (1 E)
     (0 H)) |}] in
  test (1, E);
  let%bind () = [%expect {|
    ((1 D)
     (1 J)
     (0 H)
     (0 N)) |}] in
  test (0, N);
  let%bind () =
    [%expect
      {|
    ((0 I)
     (0 O)
     (0 S)
     (1 E)
     (1 J)
     (1 O)
     (1 T)
     (1 Y)) |}]
  in
  test (1, N);
  let%bind () =
    [%expect
      {|
    ((1 I)
     (1 O)
     (1 S)
     (2 E)
     (2 J)
     (2 O)
     (2 T)
     (2 Y)) |}]
  in
  return ()
;;

let b () =
  let%bind grid = input () in
  let bugs =
    Array.foldi
      grid
      ~init:(Set.empty (module Position))
      ~f:(fun i accum row ->
        Array.foldi row ~init:accum ~f:(fun j accum bool ->
          if bool
          then Set.add accum (0, Position.Within_level.of_int_exn ((5 * i) + j))
          else accum))
  in
  let bugs = Fn.apply_n_times ~n:200 step bugs in
  printf "%d\n" (Set.length bugs);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 1902 |}]
;;
