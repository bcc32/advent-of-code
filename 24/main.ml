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
      | '.' -> false
      | '#' -> true
      | c -> failwithf "unrecognized char: %c" c ()))
  |> Bitmap.of_grid
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

let%expect_test "a" =
  let%bind bitmap = input () in
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
  let%bind bitmap = input () in
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

let b () =
  let%bind _input = input () in
  printf "output\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| output |}]
;;
