open! Core

module T = Tuple.Hashable(Int)(Int)

let visited = T.Hash_set.create ()

let rec dfs array i j =
  if i >= 0 && i < 128 && j >= 0 && j < 128
     && array.( i ).( j )
     && not (Hash_set.mem visited (i, j))
  then (
    Hash_set.add visited (i, j);
    dfs array i (j - 1);
    dfs array i (j + 1);
    dfs array (i - 1) j;
    dfs array (i + 1) j)
;;

let hash string =
  let lengths =
    (string
     |> String.to_list
     |> List.map ~f:Char.to_int)
    @ [ 17; 31; 73; 47; 23 ]
  in
  let list = Array.init 256 ~f:Fn.id in
  let pos = ref 0 in
  let skip = ref 0 in
  for _ = 1 to 64 do
    List.iter lengths ~f:(fun len ->
      for i = 0 to len / 2 - 1 do
        let left = !pos + i in
        let right = !pos + (len - i - 1) in
        let left, right = left % 256, right % 256 in
        let t = list.( left ) in
        list.( left ) <- list.( right );
        list.( right ) <- t
      done;
      pos := !pos + len + !skip;
      incr skip);
  done;
  List.range 0 16
  |> List.map ~f:(fun i -> Array.sub list ~pos:(16 * i) ~len:16)
  |> List.map ~f:(Array.fold ~init:0 ~f:(lxor))
  |> List.map ~f:(fun byte ->
    List.range 0 8
    |> List.filter ~f:(fun pos -> byte land (1 lsl (7 - pos)) <> 0))
;;

let () =
  let key =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_all
    |> String.strip
  in
  let array = Array.make_matrix false ~dimx:128 ~dimy:128 in
  for i = 0 to 127 do
    let input = key ^ "-" ^ Int.to_string i in
    let hash = hash input in
    List.iteri hash ~f:(fun j xs ->
      List.iter xs ~f:(fun x ->
        let pos = 8 * j + x in
        array.( i ).( pos ) <- true))
  done;
  let count = ref 0 in
  for i = 0 to 127 do
    for j = 0 to 127 do
      if array.( i ).( j )
      && not (Hash_set.mem visited (i, j))
      then (
        dfs array i j;
        incr count)
    done
  done;
  printf "%d\n" !count
;;
