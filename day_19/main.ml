open! Core
open! Async
open! Import

let parse_line =
  let re =
    let open Re in
    compile (seq [ group (rep1 wordc); str " => "; group (rep1 wordc) ])
  in
  fun line ->
    let group = Re.exec re line in
    let lhs = Re.Group.get group 1 in
    let rhs = Re.Group.get group 2 in
    lhs, rhs
;;

let parse_lines lines =
  let replacements, rest =
    List.split_while lines ~f:(Fn.non String.is_empty)
    |> Tuple2.map_fst ~f:(List.map ~f:parse_line)
  in
  let rest = List.tl_exn rest in
  let init = List.hd_exn rest in
  replacements, init
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| parse_lines)
;;

let do_all_replacements replacements init =
  let results = String.Hash_set.create () in
  List.iter replacements ~f:(fun (lhs, rhs) ->
    let lhs_pat =
      let open Re in
      compile (str lhs)
    in
    Re.Seq.all lhs_pat init
    |> Seq.iter (fun group ->
      let before = String.sub init ~pos:0 ~len:(Re.Group.start group 0) in
      let after = String.subo init ~pos:(Re.Group.stop group 0) in
      let s = before ^ rhs ^ after in
      Hash_set.add results s));
  results
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let replacements, init = input in
  let set = do_all_replacements replacements init in
  print_s [%sexp (Hash_set.length set : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 509 |}] in
  return ()
;;

(* FIXME: too slow  *)

(* let count_occurrences string ~inner = *)
(*   Re.Seq.matches inner string |> Sequence.of_seq |> Sequence.length *)
(* ;; *)

(* let bfs ~get_neighbors ~start ~goal ~terminal_elements = *)
(*   let terminal_elements = *)
(*     Set.to_list terminal_elements |> List.map ~f:(fun s -> Re.compile (Re.str s)) *)
(*   in *)
(*   let should_abandon string = *)
(*     String.length string > String.length goal *)
(*     || List.exists terminal_elements ~f:(fun inner -> *)
(*       count_occurrences string ~inner > count_occurrences goal ~inner) *)
(*   in *)
(*   let queue = Queue.of_list [ start ] in *)
(*   let dist = String.Table.of_alist_exn [ start, 0 ] in *)
(*   with_return (fun { return } -> *)
(*     while not (Queue.is_empty queue) do *)
(*       let x = Queue.dequeue_exn queue in *)
(*       let d = Hashtbl.find_exn dist x in *)
(*       if String.equal x goal then return d; *)
(*       get_neighbors x *)
(*       |> List.iter ~f:(fun y -> *)
(*         if (not (Hashtbl.mem dist y)) && not (should_abandon y) *)
(*         then ( *)
(*           Queue.enqueue queue y; *)
(*           Hashtbl.set dist ~key:y ~data:(d + 1))) *)
(*     done; *)
(*     failwith "can't find goal") *)
(* ;; *)

(* let b () = *)
(*   let%bind input = Lazy_deferred.force_exn input in *)
(*   let replacements, goal = input in *)
(*   let terminal_elements = *)
(*     let lhs_elements = *)
(*       List.map replacements ~f:(fun (lhs, _) -> lhs) |> String.Set.of_list *)
(*     in *)
(*     let rhs_elements = *)
(*       let pat = *)
(*         let open Re in *)
(*         compile (seq [ upper; opt lower ]) *)
(*       in *)
(*       List.concat_map replacements ~f:(fun (_, rhs) -> Re.matches pat rhs) *)
(*       |> String.Set.of_list *)
(*     in *)
(*     Set.diff rhs_elements lhs_elements *)
(*   in *)
(*   Debug.eprint_s [%message (terminal_elements : String.Set.t)]; *)
(*   let do_all_replacements init = *)
(*     do_all_replacements replacements init |> Hash_set.to_list *)
(*   in *)
(*   let _ = goal in *)
(*   let dist = *)
(*     bfs ~get_neighbors:do_all_replacements ~start:"e" ~goal:"NAl" ~terminal_elements *)
(*   in *)
(*   print_s [%sexp (dist : int)]; *)
(*   return () *)
(* ;; *)

(* let%expect_test "b" = *)
(*   let%bind () = b () in *)
(*   let%bind () = [%expect {| *)
     (*     (terminal_elements (Ar C Rn Y)) *)
     (*     1 |}] in *)
(*   return () *)
(* ;; *)
