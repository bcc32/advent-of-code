open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| (String.split_lines >> List.map ~f:Int.of_string >> List.rev))
;;

let iter_balanced list ~f ~factor =
  let group_weight = List.sum (module Int) list ~f:Fn.id / factor in
  let rec loop_incl_excl list ~included ~excluded ~included_sum ~f =
    if included_sum <= group_weight
    then (
      match list with
      | [] -> if included_sum = group_weight then f ~included ~excluded
      | hd :: tl ->
        loop_incl_excl
          tl
          ~included:(hd :: included)
          ~excluded
          ~included_sum:(hd + included_sum)
          ~f;
        loop_incl_excl tl ~included ~excluded:(hd :: excluded) ~included_sum ~f)
  in
  loop_incl_excl
    list
    ~included:[]
    ~excluded:[]
    ~included_sum:0
    ~f:(fun ~included:front_group ~excluded ->
      let any_balance =
        with_return (fun { return } ->
          loop_incl_excl
            excluded
            ~included:[]
            ~excluded:[]
            ~included_sum:0
            ~f:(fun ~included:_ ~excluded:_ -> return true);
          false)
      in
      match any_balance with
      | false -> ()
      | true -> f ~front_group)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let front_groups = Queue.create () in
  iter_balanced
    input
    ~f:(fun ~front_group -> Queue.enqueue front_groups front_group)
    ~factor:3;
  let front_groups = Queue.to_list front_groups in
  let best =
    List.min_elt
      front_groups
      ~compare:
        (Comparable.lexicographic
           [ Comparable.lift ~f:List.length Int.compare
           ; Comparable.lift ~f:(List.fold ~init:1 ~f:( * )) Int.compare
           ])
    |> Option.value_exn
  in
  let ans = List.reduce_exn best ~f:( * ) in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 11266889531 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let front_groups = Queue.create () in
  iter_balanced
    input
    ~f:(fun ~front_group -> Queue.enqueue front_groups front_group)
    ~factor:4;
  let front_groups = Queue.to_list front_groups in
  let best =
    List.min_elt
      front_groups
      ~compare:
        (Comparable.lexicographic
           [ Comparable.lift ~f:List.length Int.compare
           ; Comparable.lift ~f:(List.fold ~init:1 ~f:( * )) Int.compare
           ])
    |> Option.value_exn
  in
  let ans = List.reduce_exn best ~f:( * ) in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 77387711 |}];
  return ()
;;
