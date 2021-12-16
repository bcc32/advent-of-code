open! Core

let () =
  let layers =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      let index = String.index_exn line ':' in
      let depth = line |> String.subo ~len:index |> Int.of_string in
      let range = line |> String.subo ~pos:(index + 2) |> Int.of_string in
      depth, range)
    |> Int.Map.of_alist_exn
  in
  let end_, _ = Map.max_elt_exn layers in
  let scanners = Array.create 0 ~len:(end_ + 1) in
  let severity = ref 0 in
  for i = 0 to end_ do
    (match Map.find layers i with
     | None -> ()
     | Some range ->
       if scanners.(i) % (2 * (range - 1)) = 0
       then (
         Debug.eprint_s [%message (i : int) (range : int)];
         severity := !severity + (i * range)));
    for j = 0 to Array.length scanners - 1 do
      match Map.find layers j with
      | None -> ()
      | Some _ -> scanners.(j) <- scanners.(j) + 1
    done
  done;
  printf "%d\n" !severity
;;
