open! Core

let pattern =
  Re.Perl.compile_pat
    "^p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, \
     a=<(-?\\d+),(-?\\d+),(-?\\d+)>$"
;;

let step ((px, py, pz), (vx, vy, vz), (ax, ay, az)) =
  let vx = vx + ax in
  let vy = vy + ay in
  let vz = vz + az in
  let px = px + vx in
  let py = py + vy in
  let pz = pz + vz in
  (px, py, pz), (vx, vy, vz), (ax, ay, az)
;;

let collide particles =
  let table = Hashtbl.Poly.create () in
  List.iter particles ~f:(fun (pos, _, _) ->
    Hashtbl.update table pos ~f:(fun x -> Option.value x ~default:0 + 1));
  List.filter particles ~f:(fun (pos, _, _) -> Hashtbl.find_exn table pos < 2)
;;

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      match Re.exec pattern line |> Re.Group.all with
      | [| _; px; py; pz; vx; vy; vz; ax; ay; az |] ->
        ( (Int.of_string px, Int.of_string py, Int.of_string pz)
        , (Int.of_string vx, Int.of_string vy, Int.of_string vz)
        , (Int.of_string ax, Int.of_string ay, Int.of_string az) )
      | x -> raise_s [%message "invalid" (line : string) (x : string array)])
  in
  let rec loop i particles =
    if i = 10_000 then particles else loop (i + 1) (List.map particles ~f:step |> collide)
  in
  loop 1 input |> List.length |> printf "%d\n"
;;
