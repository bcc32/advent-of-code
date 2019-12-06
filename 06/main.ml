open! Core
open! Async
open! Import

let input () = Reader.file_lines "input" >>| List.map ~f:(String.lsplit2_exn ~on:')')

let orbits pairs =
  let graph = Graph.of_edges pairs ~hashable:String.hashable ~edge_kind:`Directed in
  let rec indirectly_and_directly_orbiting =
    let cache = String.Table.create () in
    fun inner ->
      Hashtbl.findi_or_add cache inner ~default:(fun inner ->
        Graph.outgoing_edges graph inner
        |> List.concat_map ~f:(fun next ->
          next :: indirectly_and_directly_orbiting next)
        |> List.dedup_and_sort ~compare:[%compare: string])
  in
  List.sum
    (module Int)
    (Graph.node_list graph |> ok_exn)
    ~f:(fun inner -> indirectly_and_directly_orbiting inner |> List.length)
;;

let a () =
  let%bind input = input () in
  orbits input |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 314247 |}]
;;

let orbits pairs =
  let graph = Graph.of_edges pairs ~hashable:String.hashable ~edge_kind:`Undirected in
  let start = Graph.incoming_edges graph "YOU" |> List.hd_exn in
  let end_ = Graph.incoming_edges graph "SAN" |> List.hd_exn in
  let distance = unstage (Graph.bfs graph ~start) in
  distance end_ |> Option.value_exn
;;

let b () =
  let%bind input = input () in
  orbits input |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 514 |}]
;;
