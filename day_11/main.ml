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
  type t = State.t array array [@@deriving sexp_of]

  let parse input : t =
    input
    |> String.split_lines
    |> Array.of_list_map ~f:(fun s -> String.to_array s |> Array.map ~f:State.of_char)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let do_round grid =
  let new_grid = Array.map grid ~f:Array.copy in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let num_occupied = ref 0 in
      for i = row - 1 to row + 1 do
        for j = col - 1 to col + 1 do
          if i >= 0
          && i < rows
          && j >= 0
          && j < cols
          && (i <> row || j <> col)
          && State.is_occupied grid.(i).(j)
          then incr num_occupied
        done
      done;
      match grid.(row).(col) with
      | Empty -> if !num_occupied = 0 then new_grid.(row).(col) <- Occupied
      | Occupied -> if !num_occupied >= 4 then new_grid.(row).(col) <- Empty
      | Floor -> ()
    done
  done;
  new_grid
;;

let a () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  let final_grid =
    let rec loop grid new_grid =
      if [%equal: State.t array array] grid new_grid
      then grid
      else loop new_grid (do_round new_grid)
    in
    loop grid (do_round grid)
  in
  Array.sum (module Int) final_grid ~f:(Array.count ~f:State.is_occupied)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 2483 |}] in
  return ()
;;

(*
   A B C D
   E F G H
*)
let do_round (grid : Input.t) =
  let new_grid = Array.map grid ~f:Array.copy in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let count_occupied = Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> 0)) in
  let rec loop_incrementing_occupied_count
            row
            col
            (most_recently_seen_seat : State.t)
            dr
            dc
    =
    if row >= 0 && row < rows && col >= 0 && col < cols
    then (
      let this_seat =
        match grid.(row).(col) with
        | Floor -> most_recently_seen_seat
        | seat -> seat
      in
      (match most_recently_seen_seat with
       | Empty | Floor -> ()
       | Occupied -> count_occupied.(row).(col) <- count_occupied.(row).(col) + 1);
      loop_incrementing_occupied_count (row + dr) (col + dc) this_seat dr dc)
    else row - dr, col - dc
  in
  for row = 0 to rows - 1 do
    ignore (loop_incrementing_occupied_count row 0 Floor 0 1);
    ignore (loop_incrementing_occupied_count row (cols - 1) Floor 0 (-1))
  done;
  for col = 0 to cols - 1 do
    ignore (loop_incrementing_occupied_count 0 col Floor 1 0);
    ignore (loop_incrementing_occupied_count (rows - 1) col Floor (-1) 0)
  done;
  (* 0 diagonal is NW corner down

     1 2 3 4
     5 6 7 8

     diag = 1
  *)
  for row = rows - 1 downto 0 do
    let r, c = loop_incrementing_occupied_count row 0 Floor 1 1 in
    ignore (loop_incrementing_occupied_count r c Floor (-1) (-1))
  done;
  for col = 1 to cols - 1 do
    let r, c = loop_incrementing_occupied_count 0 col Floor 1 1 in
    ignore (loop_incrementing_occupied_count r c Floor (-1) (-1))
  done;
  (* 0 diagonal is NE corner down

     1 2 3 4
     5 6 7 8

     diag = 1
  *)
  for row = rows - 1 downto 0 do
    let r, c = loop_incrementing_occupied_count row (cols - 1) Floor 1 (-1) in
    ignore (loop_incrementing_occupied_count r c Floor (-1) 1)
  done;
  for col = 0 to cols - 2 do
    let r, c = loop_incrementing_occupied_count 0 col Floor 1 (-1) in
    ignore (loop_incrementing_occupied_count r c Floor (-1) 1)
  done;
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let num_occupied = count_occupied.(row).(col) in
      match grid.(row).(col) with
      | Empty -> if num_occupied = 0 then new_grid.(row).(col) <- Occupied
      | Occupied -> if num_occupied >= 5 then new_grid.(row).(col) <- Empty
      | Floor -> ()
    done
  done;
  new_grid
;;

let b () =
  let%bind grid = Lazy_deferred.force_exn Input.t in
  let final_grid =
    let rec loop grid new_grid =
      if [%equal: State.t array array] grid new_grid
      then grid
      else loop new_grid (do_round new_grid)
    in
    loop grid (do_round grid)
  in
  Array.sum (module Int) final_grid ~f:(Array.count ~f:State.is_occupied)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {|
    2285 |}] in
  return ()
;;
