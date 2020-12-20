open! Core
open! Async
open! Import

module Tile = struct
  open Grid.O

  type t = char Grid.t [@@deriving sexp_of]

  let flip_horiz t =
    Grid.with_dimensions
      t
      ~f:
        (Grid.init ~f:(fun coord ->
           let r, c = Coord.RC.to_pair coord in
           let c = Grid.width t - c - 1 in
           t.%(Coord.RC.create ~row:r ~col:c)))
  ;;

  let rotate_ccw t =
    Grid.with_dimensions
      t
      ~f:
        (Grid.init ~f:(fun coord ->
           let r, c = Coord.RC.to_pair coord in
           let r, c = c, Grid.width t - r - 1 in
           t.%(Coord.RC.create ~row:r ~col:c)))
  ;;

  let all_rotations t =
    [ Fn.id
    ; flip_horiz
    ; rotate_ccw
    ; rotate_ccw >> flip_horiz
    ; rotate_ccw >> rotate_ccw
    ; rotate_ccw >> rotate_ccw >> flip_horiz
    ; rotate_ccw >> rotate_ccw >> rotate_ccw
    ; rotate_ccw >> rotate_ccw >> rotate_ccw >> flip_horiz
    ]
    |> List.map ~f:(fun f -> f t)
  ;;

  let does_align_horiz ~left ~right =
    let width = Grid.width left in
    let height = Grid.height left in
    with_return (fun { return } ->
      for row = 0 to height - 1 do
        if Char.( <> )
             left.%(Coord.RC.create ~row ~col:(width - 1))
             right.%(Coord.RC.create ~row ~col:0)
        then return false
      done;
      true)
  ;;

  let does_align_vert ~top ~bottom =
    let width = Grid.width top in
    let height = Grid.height top in
    with_return (fun { return } ->
      for col = 0 to width - 1 do
        if Char.( <> )
             top.%(Coord.RC.create ~row:(height - 1) ~col)
             bottom.%(Coord.RC.create ~row:0 ~col)
        then return false
      done;
      true)
  ;;
end

let%expect_test "rotate_ccw" =
  let g =
    Grid.init ~width:2 ~height:2 ~f:(fun rc ->
      let r, c = Coord.RC.to_pair rc in
      Char.of_int_exn (Char.to_int '0' + ((2 * r) + c)))
  in
  let g' = Tile.rotate_ccw g in
  print_s [%message (g : Tile.t) (g' : Tile.t)];
  (* 0 1
     2 3

     1 3
     0 2 *)
  [%expect {|
    ((g  ((0 1) (2 3)))
     (g' ((1 3) (0 2)))) |}]
;;

let%expect_test "all_rotations" =
  let g =
    Grid.init ~width:2 ~height:2 ~f:(fun rc ->
      let r, c = Coord.RC.to_pair rc in
      "PWGB".[(2 * r) + c])
  in
  Tile.all_rotations g |> [%sexp_of: Tile.t list] |> print_s;
  (* http://facstaff.cbu.edu/wschrein/media/M402%20Notes/M402C1.pdf *)
  [%expect
    {|
    (((P W) (G B))
     ((W P) (B G))
     ((W B) (P G))
     ((B W) (G P))
     ((B G) (W P))
     ((G B) (P W))
     ((G P) (B W))
     ((P G) (W B))) |}]
;;

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Tile.t Int.Map.t [@@deriving sexp_of]

  let parse input : t =
    input
    |> lines
    |> paragraphs
    |> List.filter ~f:(not << List.is_empty)
    |> List.map ~f:(fun para ->
      match para with
      | hd :: tl ->
        let tile_number = Scanf.sscanf hd "Tile %d:" (fun n -> n) in
        let grid =
          Grid.of_matrix_exn
            (Array.of_list_map ~f:(fun line -> String.to_array line) tl)
        in
        tile_number, grid
      | [] -> failwith "parse")
    |> Int.Map.of_alist_exn
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let does_align image =
  let open Grid.O in
  Grid.Lines.rows image `Top_to_bottom `Left_to_right
  |> Sequence.for_all ~f:(fun row_coords ->
    Sequence.zip row_coords (Sequence.tl_eagerly_exn row_coords)
    |> Sequence.for_all ~f:(fun (left, right) ->
      Tile.does_align_horiz
        ~left:(snd image.%(left))
        ~right:(snd image.%(right))))
  && Grid.Lines.cols image `Left_to_right `Top_to_bottom
     |> Sequence.for_all ~f:(fun col_coords ->
       Sequence.zip col_coords (Sequence.tl_eagerly_exn col_coords)
       |> Sequence.for_all ~f:(fun (top, bottom) ->
         Tile.does_align_vert
           ~top:(snd image.%(top))
           ~bottom:(snd image.%(bottom))))
;;

exception Done

let find_arrangement_exn tiles =
  let image_side_length_in_tiles =
    Float.sqrt (float (Map.length tiles)) |> Float.iround_nearest_exn
  in
  let working =
    Grid.init
      ~width:image_side_length_in_tiles
      ~height:image_side_length_in_tiles
      ~f:(Fn.const (0, Map.data tiles |> List.hd_exn))
  in
  let used_tiles = Hash_set.create (module Int) in
  let open Grid.O in
  let rec loop row col =
    if false
    then
      Debug.eprint_s
        [%message (row : int) (col : int) ~grid:(Grid.map working ~f:fst : int Grid.t)];
    if row < image_side_length_in_tiles
    then
      if col < image_side_length_in_tiles
      then
        Map.iteri tiles ~f:(fun ~key:id ~data:tile ->
          if not (Hash_set.mem used_tiles id)
          then
            List.iter (Tile.all_rotations tile) ~f:(fun tile ->
              let does_align_left =
                if col > 0
                then
                  Tile.does_align_horiz
                    ~left:(snd working.%(Coord.RC.create ~row ~col:(col - 1)))
                    ~right:tile
                else true
              in
              let does_align_up =
                if row > 0
                then
                  Tile.does_align_vert
                    ~top:(snd working.%(Coord.RC.create ~row:(row - 1) ~col))
                    ~bottom:tile
                else true
              in
              if does_align_left && does_align_up
              then (
                Hash_set.add used_tiles id;
                working.%(Coord.RC.create ~row ~col) <- id, tile;
                loop row (col + 1);
                working.%(Coord.RC.create ~row ~col) <- 0, tile;
                Hash_set.remove used_tiles id)))
      else loop (row + 1) 0
    else raise Done
  in
  try
    loop 0 0;
    failwith "failed"
  with
  | Done -> working, image_side_length_in_tiles
;;

let a () =
  let%bind tiles = Lazy_deferred.force_exn Input.t in
  let working, image_side_length_in_tiles = find_arrangement_exn tiles in
  print_s [%sexp (Grid.map working ~f:fst : int Grid.t)];
  let corners =
    [ Coord.RC.create ~row:0 ~col:0
    ; Coord.RC.create ~row:0 ~col:(image_side_length_in_tiles - 1)
    ; Coord.RC.create ~row:(image_side_length_in_tiles - 1) ~col:0
    ; Coord.RC.create
        ~row:(image_side_length_in_tiles - 1)
        ~col:(image_side_length_in_tiles - 1)
    ]
    |> List.map ~f:(fun rc -> Grid.get_exn working rc)
    |> List.map ~f:fst
    |> List.reduce_exn ~f:( * )
  in
  print_s [%sexp (corners : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () =
    [%expect
      {|
    ((1447 3041 2069 3457 1789 3943 2011 1409 1091 1549 1669 3221)
     (3583 3329 1777 1319 2389 3833 1171 1031 2749 3019 2707 3821)
     (1867 3347 3037 1933 2927 1499 1151 1481 3253 1489 3373 3769)
     (1423 2467 2383 2203 1373 1451 3539 3533 1213 2381 3169 3931)
     (3659 1123 2837 2267 1543 3167 1993 2879 1277 3637 3967 2417)
     (1399 1019 3863 2521 2633 2309 2687 2861 1427 3299 1009 1039)
     (2957 2027 2357 1103 2683 1129 1381 1973 3229 1693 3527 3433)
     (3319 2113 3079 3209 1637 3271 3187 1663 3313 1699 3877 3919)
     (3023 2851 3413 1063 2663 3631 3613 2699 3391 2371 3469 3307)
     (2131 2411 2897 2857 2377 2971 1997 2711 2213 1459 1697 3491)
     (1249 3089 2731 3001 3853 2243 3547 1831 1231 2789 3407 2153)
     (1873 2621 1523 3607 1811 3739 2473 2441 2647 1601 3793 2029))
    17712468069479 |}]
  in
  return ()
;;

let sea_monster =
  {|
                  # %
#    ##    ##    ###%
 #  #  #  #  #  #   %
|}
  |> String.strip ~drop:Char.(( = ) '\n')
  |> String.split_lines
  |> Array.of_list_map ~f:(Fn.flip String.drop_suffix 1 >> String.to_array)
  |> Grid.of_matrix_exn
;;

let is_subgrid_at grid ~subgrid ~row ~col =
  let open Grid.O in
  if row + Grid.height subgrid > Grid.height grid
  || col + Grid.width subgrid > Grid.width grid
  then false
  else
    Grid.Lines.rows subgrid `Top_to_bottom `Left_to_right
    |> Sequence.concat
    |> Sequence.for_all ~f:(fun rc ->
      let rc_in_larger_grid =
        let r, c = Coord.RC.to_pair rc in
        Coord.RC.create ~row:(row + r) ~col:(col + c)
      in
      match grid.%(rc_in_larger_grid), subgrid.%(rc) with
      | _, ' ' -> true
      | '#', '#' -> true
      | _, '#' -> false
      | _, c -> failwithf "sea monster bad char: %c" c ())
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let working, image_side_length_in_tiles = find_arrangement_exn input in
  let tiles =
    Grid.map working ~f:(fun (_id, tile) ->
      let tile = (tile :> char array array) in
      Array.sub tile ~pos:1 ~len:(Array.length tile - 2)
      |> Array.map ~f:(fun row -> Array.sub row ~pos:1 ~len:(Array.length row - 2))
      |> Grid.of_matrix_exn)
  in
  let single_tile_height, single_tile_width =
    (tiles : Tile.t Grid.t :> Tile.t array array).(0).(0)
    |> Grid.with_dimensions ~f:(fun ~width ~height -> height, width)
  in
  let open Grid.O in
  let image =
    Grid.init
      ~width:(single_tile_width * image_side_length_in_tiles)
      ~height:(single_tile_height * image_side_length_in_tiles)
      ~f:(fun rc ->
        let r, c = Coord.RC.to_pair rc in
        let r1, r2 = r / single_tile_height, r % single_tile_height in
        let c1, c2 = c / single_tile_width, c % single_tile_width in
        tiles.%(Coord.RC.create ~row:r1 ~col:c1).%(Coord.RC.create ~row:r2 ~col:c2))
  in
  List.iter (Tile.all_rotations image) ~f:(fun image ->
    let there_were_sea_monsters = ref false in
    for r = 0 to Grid.height image - 1 do
      for c = 0 to Grid.width image - 1 do
        if is_subgrid_at image ~subgrid:sea_monster ~row:r ~col:c
        then (
          there_were_sea_monsters := true;
          for r' = 0 to Grid.height sea_monster - 1 do
            for c' = 0 to Grid.width sea_monster - 1 do
              if Char.( = ) '#' sea_monster.%(Coord.RC.create ~row:r' ~col:c')
              then (
                let coord = Coord.RC.create ~row:(r + r') ~col:(c + c') in
                image.%(coord) <- 'O')
            done
          done)
      done
    done;
    if !there_were_sea_monsters
    then Grid.Row_major.count image ~f:(Char.( = ) '#') |> [%sexp_of: int] |> print_s);
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 2173 |}] in
  return ()
;;
