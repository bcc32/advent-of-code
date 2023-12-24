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
    Reader.file_contents "aoc.in" >>| String.split_lines >>| parse_lines)
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
  [%expect {| 509 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let replacements, start = input in
  let replacements =
    List.sort
      replacements
      ~compare:(Comparable.lift ~f:snd (Comparable.lift ~f:String.length Int.descending))
    |> List.map ~f:(Tuple2.map_snd ~f:(fun rhs -> String.Search_pattern.create rhs))
  in
  let queue =
    Pairing_heap.create
      ~cmp:(Comparable.lift ~f:fst (Comparable.lift ~f:String.length Int.compare))
      ()
  in
  Pairing_heap.add queue (start, 0);
  let ans =
    with_return (fun { return } ->
      while not (Pairing_heap.is_empty queue) do
        let start, dist = Pairing_heap.pop_exn queue in
        if String.equal start "e" then return dist;
        List.iter replacements ~f:(fun (lhs, rhs) ->
          Option.iter (String.Search_pattern.index rhs ~in_:start) ~f:(fun index ->
            let rhs_len = String.length (String.Search_pattern.pattern rhs) in
            let str =
              String.sub start ~pos:0 ~len:index
              ^ lhs
              ^ String.drop_prefix start (index + rhs_len)
            in
            Pairing_heap.add queue (str, dist + 1)))
      done;
      assert false)
  in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 195 |}];
  return ()
;;
