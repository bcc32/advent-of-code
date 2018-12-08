open! Core
open! Async
open! Import

let re =
  Re.(
    compile
      (seq
         [ str "Step "
         ; group upper
         ; str " must be finished before step "
         ; group upper
         ; str " can begin."
         ]))
;;

let of_string line =
  let g = Re.exec re line in
  let src = Re.Group.get g 1 in
  let dst = Re.Group.get g 2 in
  src, dst
;;

let topo_sort deps =
  let all_nodes =
    List.concat_map deps ~f:(fun (x, y) -> [ x; y ])
    |> List.dedup_and_sort ~compare:String.compare
  in
  let clients =
    List.fold deps ~init:String.Map.empty ~f:(fun acc (src, dst) ->
      Map.add_multi acc ~key:src ~data:dst)
  in
  let remaining_deps =
    let tbl = String.Table.create () in
    List.iter deps ~f:(fun (src, dst) ->
      let set = Hashtbl.find_or_add tbl dst ~default:String.Hash_set.create in
      Hash_set.add set src);
    tbl
  in
  let ready =
    let heap = Heap.create () ~cmp:String.compare in
    List.iter all_nodes ~f:(fun node ->
      if not (Hashtbl.mem remaining_deps node) then Heap.add heap node);
    heap
  in
  let result = ref [] in
  while not (Heap.is_empty ready) do
    let step = Heap.pop_exn ready in
    let clients = Map.find_multi clients step in
    List.iter clients ~f:(fun c ->
      let set = Hashtbl.find_exn remaining_deps c in
      Hash_set.remove set step;
      if Hash_set.is_empty set then Heap.add ready c);
    result := step :: !result
  done;
  List.rev !result
;;
