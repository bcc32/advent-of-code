open! Core
open! Async
open! Import

module Input = struct
  type t = (int * int) list [@@deriving sexp_of]

  let parse input : t =
    input
    |> String.split_lines
    |> List.map ~f:(fun line ->
      let row =
        String.sub line ~pos:0 ~len:7
        |> String.fold ~init:0 ~f:(fun acc digit ->
          if Char.( = ) digit 'F' then 2 * acc else (2 * acc) + 1)
      in
      let col =
        String.sub line ~pos:7 ~len:3
        |> String.fold ~init:0 ~f:(fun acc digit ->
          if Char.( = ) digit 'L' then 2 * acc else (2 * acc) + 1)
      in
      row, col)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let seat_id (row, col) = (row * 8) + col

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  List.map input ~f:seat_id
  |> List.max_elt ~compare:Int.compare
  |> uw
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 919 |}] in
  return ()
;;

let b () =
  let%bind seats = Lazy_deferred.force_exn Input.t in
  let ids = List.map seats ~f:seat_id |> Set.of_list (module Int) in
  let missing_id =
    let rec loop i =
      if Set.mem ids (i - 1) && Set.mem ids (i + 1) && not (Set.mem ids i)
      then i
      else loop (i + 1)
    in
    loop 1
  in
  print_s [%sexp (missing_id : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 642 |}] in
  return ()
;;
