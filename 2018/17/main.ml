open! Core
open! Async
open! Import

module Coord = struct
  type t = int * int [@@deriving compare, equal, hash, sexp_of]
end

let input =
  let pattern =
    lazy (Re.Perl.compile_pat {|x=(\d+), y=(\d+)..(\d+)|y=(\d+), x=(\d+)..(\d+)|})
  in
  Lazy_deferred.create (fun () ->
    let clay = Hash_set.create (module Coord) in
    let%bind lines = Reader.file_lines "aoc.in" in
    List.iter lines ~f:(fun line ->
      let group = Re.exec (force pattern) line in
      if Re.Group.test group 1
      then (
        let x = Int.of_string (Re.Group.get group 1) in
        let y1 = Int.of_string (Re.Group.get group 2) in
        let y2 = Int.of_string (Re.Group.get group 3) in
        assert (y1 <= y2);
        for y = y1 to y2 do
          Hash_set.add clay (x, y)
        done)
      else (
        let y = Int.of_string (Re.Group.get group 4) in
        let x1 = Int.of_string (Re.Group.get group 5) in
        let x2 = Int.of_string (Re.Group.get group 6) in
        assert (x1 <= x2);
        for x = x1 to x2 do
          Hash_set.add clay (x, y)
        done));
    return clay)
;;

module Scan_result = struct
  type t =
    | Fall_forever
    | Filled_space
end

let rec scan_down x y ~clay ~max_y ~(visited : (int * int, Scan_result.t) Hashtbl.t)
  : Scan_result.t
  =
  if Hash_set.mem clay (x, y)
  then Filled_space
  else
    Hashtbl.find_or_add visited (x, y) ~default:(fun () ->
      if y > max_y
      then Fall_forever
      else (
        match scan_down x (y + 1) ~clay ~max_y ~visited with
        | Fall_forever -> Fall_forever
        | Filled_space ->
          let left_result, min_x = scan_left (x - 1) y ~clay ~max_y ~visited in
          let right_result, max_x = scan_right (x + 1) y ~clay ~max_y ~visited in
          (match left_result, right_result with
           | Filled_space, Filled_space -> Filled_space
           | Fall_forever, _ | _, Fall_forever ->
             (* Either left or right might be incorrectly considered filled space,
               but we need to mark it as falling because it is not bounded on the
               other side. *)
             for x = min_x to max_x do
               Hashtbl.set visited ~key:(x, y) ~data:Fall_forever
             done;
             Fall_forever)))

and scan_left x y ~clay ~max_y ~visited : Scan_result.t * int =
  if Hash_set.mem clay (x, y)
  then Filled_space, x + 1
  else (
    match Hashtbl.find visited (x, y) with
    | Some state -> state, x
    | None ->
      (match scan_down x (y + 1) ~clay ~max_y ~visited with
       | Fall_forever ->
         Hashtbl.set visited ~key:(x, y) ~data:Fall_forever;
         Fall_forever, x
       | Filled_space ->
         Hashtbl.set visited ~key:(x, y) ~data:Filled_space;
         scan_left (x - 1) y ~clay ~max_y ~visited))

and scan_right x y ~clay ~max_y ~visited : Scan_result.t * int =
  if Hash_set.mem clay (x, y)
  then Filled_space, x - 1
  else (
    match Hashtbl.find visited (x, y) with
    | Some state -> state, x
    | None ->
      (match scan_down x (y + 1) ~clay ~max_y ~visited with
       | Fall_forever ->
         Hashtbl.set visited ~key:(x, y) ~data:Fall_forever;
         Fall_forever, x
       | Filled_space ->
         Hashtbl.set visited ~key:(x, y) ~data:Filled_space;
         scan_right (x + 1) y ~clay ~max_y ~visited))
;;

let print_board ~clay ~(visited : (int * int, Scan_result.t) Hashtbl.t) ~min_y ~max_y =
  let xs = clay |> Hash_set.to_list |> List.map ~f:fst in
  let min_x = List.min_elt xs ~compare:Int.compare |> Option.value_exn in
  let max_x = List.max_elt xs ~compare:Int.compare |> Option.value_exn in
  for y = min_y to max_y do
    for x = min_x - 1 to max_x + 1 do
      let char =
        if Hash_set.mem clay (x, y)
        then '#'
        else (
          match Hashtbl.find visited (x, y) with
          | Some Filled_space -> '~'
          | Some Fall_forever -> '|'
          | None -> '.')
      in
      print_char char
    done;
    print_endline ""
  done
;;

let a () =
  let%bind clay = Lazy_deferred.force_exn input in
  let ys = clay |> Hash_set.to_list |> List.map ~f:snd in
  let min_y = List.min_elt ys ~compare:Int.compare |> Option.value_exn in
  let max_y = List.max_elt ys ~compare:Int.compare |> Option.value_exn in
  let visited = Hashtbl.create (module Coord) in
  let scan_result = scan_down 500 0 ~clay ~max_y ~visited in
  Hashtbl.filter_keys_inplace visited ~f:(fun (_, y) ->
    Int.between y ~low:min_y ~high:max_y);
  match scan_result with
  | Filled_space -> assert false
  | Fall_forever ->
    print_s [%sexp (Hashtbl.length visited : int)];
    if false then print_board ~clay ~visited ~min_y ~max_y;
    return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 31949 |}];
  return ()
;;

let b () =
  let%bind clay = Lazy_deferred.force_exn input in
  let ys = clay |> Hash_set.to_list |> List.map ~f:snd in
  let min_y = List.min_elt ys ~compare:Int.compare |> Option.value_exn in
  let max_y = List.max_elt ys ~compare:Int.compare |> Option.value_exn in
  let visited = Hashtbl.create (module Coord) in
  let scan_result = scan_down 500 0 ~clay ~max_y ~visited in
  Hashtbl.filteri_inplace visited ~f:(fun ~key:(_, y) ~data:state ->
    Int.between y ~low:min_y ~high:max_y
    &&
    match state with
    | Fall_forever -> false
    | Filled_space -> true);
  match scan_result with
  | Filled_space -> assert false
  | Fall_forever ->
    print_s [%sexp (Hashtbl.length visited : int)];
    if false then print_board ~clay ~visited ~min_y ~max_y;
    return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 26384 |}];
  return ()
;;
