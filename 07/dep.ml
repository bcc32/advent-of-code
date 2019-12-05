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
    let heap = Pairing_heap.create () ~cmp:String.compare in
    List.iter all_nodes ~f:(fun node ->
      if not (Hashtbl.mem remaining_deps node) then Pairing_heap.add heap node);
    heap
  in
  let result = ref [] in
  while not (Pairing_heap.is_empty ready) do
    let step = Pairing_heap.pop_exn ready in
    let clients = Map.find_multi clients step in
    List.iter clients ~f:(fun c ->
      let set = Hashtbl.find_exn remaining_deps c in
      Hash_set.remove set step;
      if Hash_set.is_empty set then Pairing_heap.add ready c);
    result := step :: !result
  done;
  List.rev !result
;;

let schedule deps ~workers ~cost =
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
    let heap = Pairing_heap.create () ~cmp:String.compare in
    List.iter all_nodes ~f:(fun node ->
      if not (Hashtbl.mem remaining_deps node) then Pairing_heap.add heap node);
    heap
  in
  let current_task = Array.create None ~len:workers in
  let work_left = Array.create 0 ~len:workers in
  let rec loop time =
    if Array.exists current_task ~f:Option.is_none && not (Pairing_heap.is_empty ready)
    then (
      let step = Pairing_heap.pop_exn ready in
      let i, _ = Array.findi_exn current_task ~f:(fun _ task -> Option.is_none task) in
      current_task.(i) <- Some step;
      work_left.(i) <- cost step;
      (* no work done yet *)
      loop time)
    else if Array.exists work_left ~f:(fun x -> x > 0)
    then (
      Array.map_inplace work_left ~f:pred;
      for i = 0 to workers - 1 do
        if work_left.(i) = 0
        then (
          let step = Option.value_exn current_task.(i) in
          let clients = Map.find_multi clients step in
          current_task.(i) <- None;
          List.iter clients ~f:(fun c ->
            let set = Hashtbl.find_exn remaining_deps c in
            Hash_set.remove set step;
            if Hash_set.is_empty set then Pairing_heap.add ready c))
      done;
      loop (time + 1))
    else time
  in
  loop 0
;;
