open! Core
open! Async
open! Import

type t = Point.t list

let step = List.map ~f:Point.step

let span_exn (t : t) =
  match t with
  | [] -> invalid_arg "span_exn"
  | hd :: tl ->
    List.fold
      tl
      ~init:((hd.x, hd.y), (hd.x, hd.y))
      ~f:(fun ((min_x, min_y), (max_x, max_y)) { x; y; _ } ->
        (Int.min min_x x, Int.min min_y y), (Int.max max_x x, Int.max max_y y))
;;

let area_exn (t : t) =
  let (min_x, min_y), (max_x, max_y) = span_exn t in
  (max_x - min_x) * (max_y - min_y)
;;

let to_sequence t = Sequence.unfold ~init:t ~f:(fun t -> Some ((t, area_exn t), step t))

let to_string_plot t =
  let (min_x, min_y), (max_x, max_y) = span_exn t in
  (* x and y inverted *)
  let dimx = max_y - min_y + 1 in
  let dimy = max_x - min_x + 1 in
  let chars = Array.make_matrix '.' ~dimx ~dimy in
  List.iter t ~f:(fun { x; y; _ } -> chars.(max_y - y).(x - min_x) <- '#');
  (* invert y axis *)
  Array.rev_inplace chars;
  chars
  |> Array.to_list
  |> List.map ~f:(fun row ->
    row |> Array.to_list |> List.map ~f:String.of_char |> String.concat)
  |> String.concat ~sep:"\n"
;;
