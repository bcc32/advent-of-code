open! Core

let pat = Re.(compile (seq [ opt (char '-'); rep1 digit ]))

let input =
  In_channel.read_lines "aoc.in"
  |> List.map ~f:(fun line ->
    Re.all pat line
    |> List.map ~f:(fun g -> Re.Group.get g 0 |> Int.of_string)
    |> List.to_array)
  |> List.to_array
;;

let is_intersection point1 point2 ~t =
  let x1 = point1.(0) + (t * point1.(3)) in
  let y1 = point1.(1) + (t * point1.(4)) in
  let z1 = point1.(2) + (t * point1.(5)) in
  let x2 = point2.(0) + (t * point2.(3)) in
  let y2 = point2.(1) + (t * point2.(4)) in
  let z2 = point2.(2) + (t * point2.(5)) in
  x1 = x2 && y1 = y2 && z1 = z2
;;

let maybe_intersection_time point1 point2 =
  let dx = point2.(0) - point1.(0) in
  let dvx = point1.(3) - point2.(3) in
  match dvx with
  | 0 -> None
  | dvx -> Some (dx / dvx)
;;

[@@@warning "-8"]

let [| x1; y1; z1; vx1; vy1; vz1 |] = input.(0)
let [| x2; y2; z2; vx2; vy2; vz2 |] = input.(1)

[@@@warning "+8"]

let try_p0 p0 =
  let all_intersect =
    Array.for_all input ~f:(fun p ->
      match maybe_intersection_time p0 p with
      | None -> false
      | Some t -> t >= 0 && is_intersection p0 p ~t)
  in
  if all_intersect
  then (
    print_s [%sexp (p0 : int array)];
    let ans = p0.(0) + p0.(1) + p0.(2) in
    print_s [%sexp (ans : int)];
    exit 0)
;;

let try_v t1 vx0 vy0 vz0 =
  let x0 = x1 + (vx1 * t1) - (t1 * vx0) in
  let y0 = y1 + (vy1 * t1) - (t1 * vy0) in
  let z0 = z1 + (vz1 * t1) - (t1 * vz0) in
  let p0 = [| x0; y0; z0; vx0; vy0; vz0 |] in
  try_p0 p0
;;

let rec loop max_t =
  printf "%d\n%!" max_t;
  for t1 = 0 to max_t do
    for t2 = 0 to max_t do
      if t1 <> t2
      then (
        let qx = (t2 * vx2) + x2 - (t1 * vx1) - x1 in
        let qy = (t2 * vy2) + y2 - (t1 * vy1) - y1 in
        let qz = (t2 * vz2) + z2 - (t1 * vz1) - z1 in
        if qx mod (t2 - t1) = 0 && qy mod (t2 - t1) = 0 && qz mod (t2 - t1) = 0
        then (
          let vx0 = qx / (t2 - t1) in
          let vy0 = qy / (t2 - t1) in
          let vz0 = qz / (t2 - t1) in
          try_v t1 vx0 vy0 vz0))
    done
  done;
  loop (max_t + 1)
;;

loop 1
