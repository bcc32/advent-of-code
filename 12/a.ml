open! Core

let visited = Int.Hash_set.create ()

let rec dfs edges x =
  if not (Hash_set.mem visited x)
  then (
    Hash_set.add visited x;
    Map.find edges x
    |> Option.iter ~f:(List.iter ~f:(fun other ->
      dfs edges other)))
;;

let () =
  let pipes =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      let index = String.substr_index_exn line ~pattern:"<->" in
      let left = String.subo line ~len:(index - 1) in
      let right = String.subo line ~pos:(index + 4) in
      let self = Int.of_string left in
      let others =
        right
        |> String.split ~on:','
        |> List.map ~f:String.strip
        |> List.map ~f:Int.of_string
      in
      (self, others))
    |> Int.Map.of_alist_exn
  in
  dfs pipes 0;
  Hash_set.length visited
  |> printf "%d\n"
;;
