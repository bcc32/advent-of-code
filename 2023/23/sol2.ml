open! Core

let grid = In_channel.read_lines "aoc.in" |> List.to_array
let start_x = 0

let start_y =
  String.findi grid.(0) ~f:(fun _ c -> Char.equal c '.') |> Option.value_exn |> fst
;;

let end_x = Array.length grid - 1

let end_y =
  String.findi grid.(Array.length grid - 1) ~f:(fun _ c -> Char.equal c '.')
  |> Option.value_exn
  |> fst
;;

let dirs = [ -1, 0; 1, 0; 0, -1; 0, 1 ]

let is_interesting_point x y =
  List.count dirs ~f:(fun (dx, dy) ->
    let x = x + dx in
    let y = y + dy in
    match grid.(x).[y] with
    | '#' | (exception _) -> false
    | _ -> true)
  > 2
;;

module Point = struct
  type t = int * int [@@deriving compare, equal, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)

  include Hashable.Make_plain (struct
      type nonrec t = t [@@deriving compare, hash, sexp_of]
    end)
end

let interesting_points =
  [ start_x, start_y; end_x, end_y ]
  @ (List.init (Array.length grid) ~f:(fun x ->
       List.init (String.length grid.(0)) ~f:(fun y -> x, y))
     |> List.concat_map ~f:(fun list ->
       List.filter_map list ~f:(fun (x, y) ->
         if is_interesting_point x y then Some (x, y) else None)))
  |> List.to_array
;;

let interesting_points_set =
  interesting_points |> Array.to_list |> Hash_set.of_list (module Point)
;;

let shortest_distance =
  Memo.general ~hashable:Point.hashable (fun (x1, y1) ->
    let queue = Queue.create () in
    let distance = Hashtbl.create (module Point) in
    Queue.enqueue queue (x1, y1);
    Hashtbl.add_exn distance ~key:(x1, y1) ~data:0;
    while not (Queue.is_empty queue) do
      let x, y = Queue.dequeue_exn queue in
      if [%equal: Point.t] (x, y) (x1, y1)
         || not (Hash_set.mem interesting_points_set (x, y))
      then
        List.iter dirs ~f:(fun (dx, dy) ->
          match grid.(x + dx).[y + dy] with
          | exception _ -> ()
          | '#' -> ()
          | _ ->
            if not (Hashtbl.mem distance (x + dx, y + dy))
            then (
              Hashtbl.add_exn
                distance
                ~key:(x + dx, y + dy)
                ~data:(Hashtbl.find_exn distance (x, y) + 1);
              Queue.enqueue queue (x + dx, y + dy)))
    done;
    fun (x2, y2) -> Hashtbl.find distance (x2, y2))
;;

let adj =
  let t = Hashtbl.create (module Int) in
  Array.iteri interesting_points ~f:(fun i u ->
    Array.iteri interesting_points ~f:(fun j v ->
      match shortest_distance u v with
      | None -> ()
      | Some _ -> Hashtbl.add_multi t ~key:i ~data:j));
  t
;;

let rec dfs start stop visited accum =
  if visited.(start)
  then 0
  else if start = stop
  then accum
  else (
    visited.(start) <- true;
    let ans =
      Hashtbl.find_multi adj start
      |> List.map ~f:(fun next_point ->
        let d =
          shortest_distance interesting_points.(start) interesting_points.(next_point)
          |> Option.value_exn
        in
        dfs next_point stop visited (accum + d))
      |> List.max_elt ~compare:Int.compare
    in
    visited.(start) <- false;
    ans |> Option.value ~default:Int.min_value)
;;

(* runs in about 10 seconds *)
let ans = dfs 0 1 (Array.create false ~len:(Array.length interesting_points)) 0
let () = printf "%d\n" ans
