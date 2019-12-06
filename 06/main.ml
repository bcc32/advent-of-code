open! Core
open! Async
open! Import

let input () = Reader.file_lines "input" >>| List.map ~f:(String.lsplit2_exn ~on:')')

let orbits pairs =
  let direct_orbits = String.Table.create () in
  List.iter pairs ~f:(fun (inner, outer) ->
    Hashtbl.add_multi direct_orbits ~key:inner ~data:outer);
  let rec indirectly_and_directly_orbiting =
    let cache = String.Table.create () in
    fun inner ->
      Hashtbl.findi_or_add cache inner ~default:(fun inner ->
        Hashtbl.find_multi direct_orbits inner
        |> List.concat_map ~f:(fun next ->
          next :: indirectly_and_directly_orbiting next)
        |> List.dedup_and_sort ~compare:[%compare: string])
  in
  let all =
    List.concat_map pairs ~f:(fun (x, y) -> [ x; y ])
    |> List.dedup_and_sort ~compare:[%compare: string]
  in
  List.sum
    (module Int)
    all
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
  let edges = String.Table.create () in
  List.iter pairs ~f:(fun (inner, outer) ->
    Hashtbl.add_multi edges ~key:inner ~data:outer;
    Hashtbl.add_multi edges ~key:outer ~data:inner);
  let start = Hashtbl.find_exn edges "YOU" |> List.hd_exn in
  let end_ = Hashtbl.find_exn edges "SAN" |> List.hd_exn in
  let q = Queue.create () in
  Queue.enqueue q start;
  let distance = String.Table.create () in
  Hashtbl.add_exn distance ~key:start ~data:0;
  with_return (fun { return } ->
    while not (Queue.is_empty q) do
      let x = Queue.dequeue_exn q in
      if String.equal x end_ then return (Hashtbl.find_exn distance x);
      Hashtbl.find_multi edges x
      |> List.iter ~f:(fun y ->
        if not (Hashtbl.mem distance y)
        then (
          Hashtbl.add_exn distance ~key:y ~data:(Hashtbl.find_exn distance x + 1);
          Queue.enqueue q y))
    done;
    assert false)
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
