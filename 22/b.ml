open! Core

let map = Hashtbl.Poly.create ()

let get i j = Hashtbl.find_or_add map (i, j) ~default:(fun () -> '.')

let set i j x = Hashtbl.set map ~key:(i, j) ~data:x

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.to_array
  in
  Array.iteri input ~f:(fun i x ->
    x
    |> String.to_list
    |> List.iteri ~f:(fun j y ->
      Hashtbl.set map ~key:(i, j) ~data:y));
  let n = Array.length input / 2 in
  let x = ref n in
  let y = ref n in
  let dir = ref (-1, 0) in
  let infections = ref 0 in
  let left (dx, dy) = (-dy, dx) in
  let right (dx, dy) = (dy, -dx) in
  let rev (dx, dy) = (-dx, -dy) in
  let forward () =
    x := !x + fst !dir;
    y := !y + snd !dir;
  in
  for _ = 1 to 10_000_000 do
    (match get !x !y with
     | '#' ->
       dir := right !dir;
       set !x !y 'f'
     | '.' ->
       dir := left !dir;
       set !x !y 'w';
     | 'w' ->
       incr infections;
       set !x !y '#'
     | 'f' ->
       dir := rev !dir;
       set !x !y '.'
     | _ -> assert false);
    forward ()
  done;
  printf "%d\n" !infections
;;
