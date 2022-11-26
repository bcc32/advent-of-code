open! Core
open! Async
open! Import

module State = struct
  type t =
    | Empty
    | Occupied
    | Floor
  [@@deriving equal, sexp_of]

  let of_char = function
    | 'L' -> Empty
    | '#' -> Occupied
    | '.' -> Floor
    | ch -> failwithf "invalid char %c" ch ()
  ;;

  let is_occupied = function
    | Empty | Floor -> false
    | Occupied -> true
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = State.t Grid.t [@@deriving sexp_of]

  let parse = grid ~f:State.of_char

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let do_round grid =
  let open Grid.O in
  let new_grid = Grid.copy grid in
  Grid.Lines.rows grid `Top_to_bottom `Left_to_right
  |> Sequence.concat
  |> Sequence.iter ~f:(fun cell ->
    let row, col = Coord.RC.to_pair cell in
    let num_occupied = ref 0 in
    for i = row - 1 to row + 1 do
      for j = col - 1 to col + 1 do
        let neighbor = Coord.RC.create ~row:i ~col:j in
        if (not ([%equal: Coord.RC.t] cell neighbor))
        && grid.?(neighbor)
        && State.is_occupied grid.%(neighbor)
        then incr num_occupied
      done
    done;
    match grid.%(cell) with
    | Empty -> if !num_occupied = 0 then new_grid.%(cell) <- Occupied
    | Occupied -> if !num_occupied >= 4 then new_grid.%(cell) <- Empty
    | Floor -> ());
  new_grid
;;

let a () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  let final_grid =
    let rec loop grid new_grid =
      if [%equal: State.t Grid.t] grid new_grid
      then grid
      else loop new_grid (do_round new_grid)
    in
    loop grid (do_round grid)
  in
  Grid.Row_major.count final_grid ~f:State.is_occupied |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2483 |}];
  return ()
;;

let do_round (grid : Input.t) =
  let open Grid.O in
  let new_grid = Grid.copy grid in
  let count_occupied = Grid.map grid ~f:(Fn.const 0) in
  let loop_incrementing_occupied_count line =
    let most_recently_seen_seat = ref (Floor : State.t) in
    Sequence.iter line ~f:(fun cell ->
      (match !most_recently_seen_seat with
       | Empty | Floor -> ()
       | Occupied -> count_occupied.%(cell) <- count_occupied.%(cell) + 1);
      match grid.%(cell) with
      | Floor -> ()
      | (Empty | Occupied) as seat -> most_recently_seen_seat := seat)
  in
  let do_lines f major minors =
    List.iter minors ~f:(fun minor ->
      f grid major minor |> Sequence.iter ~f:loop_incrementing_occupied_count)
  in
  (* Pick an arbitrary major axis, since it doesn't matter. *)
  (* Rows *)
  do_lines Grid.Lines.rows `Top_to_bottom [ `Left_to_right; `Right_to_left ];
  (* Columns *)
  do_lines Grid.Lines.cols `Left_to_right [ `Top_to_bottom; `Bottom_to_top ];
  (* NW-SE diagonals *)
  do_lines
    Grid.Lines.backslash_diagonals
    `Bottom_left_to_top_right
    [ `Top_left_to_bottom_right; `Bottom_right_to_top_left ];
  (* NE-SW diagonals *)
  do_lines
    Grid.Lines.forward_slash_diagonals
    `Top_left_to_bottom_right
    [ `Top_right_to_bottom_left; `Bottom_left_to_top_right ];
  Grid.Lines.rows grid `Top_to_bottom `Left_to_right
  |> Sequence.concat
  |> Sequence.iter ~f:(fun cell ->
    let num_occupied = count_occupied.%(cell) in
    match grid.%(cell) with
    | Empty -> if num_occupied = 0 then new_grid.%(cell) <- Occupied
    | Occupied -> if num_occupied >= 5 then new_grid.%(cell) <- Empty
    | Floor -> ());
  new_grid
;;

let b () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  let final_grid =
    let rec loop grid new_grid =
      if [%equal: State.t Grid.t] grid new_grid
      then grid
      else loop new_grid (do_round new_grid)
    in
    loop grid (do_round grid)
  in
  Grid.Row_major.count final_grid ~f:State.is_occupied |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 2285 |}];
  return ()
;;
