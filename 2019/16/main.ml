open! Core
open! Async
open! Import

let input () =
  let%map contents = Reader.file_contents "input" >>| String.strip in
  String.to_array contents |> Array.map ~f:Char.get_digit_exn
;;

let pattern i j =
  let repeat_length = i + 1 in
  [| 0; 1; 0; -1 |].((j + 1) / repeat_length % 4)
;;

let do_phase_naive input =
  Array.mapi input ~f:(fun i _ ->
    let sum = ref 0 in
    Array.iteri input ~f:(fun j x -> sum := !sum + (x * pattern i j));
    Int.abs (!sum mod 10))
;;

let a () =
  let%bind input = input () in
  let output = Fn.apply_n_times ~n:100 do_phase_naive input in
  Array.sub output ~pos:0 ~len:8
  |> Array.map ~f:Int.to_string
  |> String.concat_array
  |> print_endline;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 40921727 |}]
;;

let do_phase_fast_only_correct_for_back_half_of_array_inplace array =
  let sum = ref 0 in
  for i = Array.length array - 1 downto 0 do
    sum := !sum + array.(i);
    array.(i) <- Int.abs (!sum mod 10)
  done
;;

let value_at_index_is_correct_despite_incorrect_algorithm array i =
  2 * i >= Array.length array - 1
;;

let%test_unit "check simplifying assumptions" =
  Base_quickcheck.Test.run_exn
    (module struct
      type t = int list [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun list ->
      let array = Array.of_list list in
      let expect = do_phase_naive array in
      let actual =
        let array = Array.copy array in
        do_phase_fast_only_correct_for_back_half_of_array_inplace array;
        array
      in
      [%test_result: int] (Array.length actual) ~expect:(Array.length expect);
      for i = 0 to Array.length actual - 1 do
        if value_at_index_is_correct_despite_incorrect_algorithm actual i
        then (
          try [%test_result: int] actual.(i) ~expect:expect.(i) with
          | exn -> Exn.reraisef exn !"value at index %d of %{sexp: int array}" i array ())
      done)
;;

let b () =
  let%bind input = input () in
  let input = Array.concat (List.init 10_000 ~f:(Fn.const input)) in
  let offset =
    Array.sub input ~pos:0 ~len:7
    |> Array.map ~f:Int.to_string
    |> String.concat_array
    |> Int.of_string
  in
  assert (value_at_index_is_correct_despite_incorrect_algorithm input offset);
  for _ = 1 to 100 do
    do_phase_fast_only_correct_for_back_half_of_array_inplace input
  done;
  Array.sub input ~pos:offset ~len:8
  |> Array.map ~f:Int.to_string
  |> String.concat_array
  |> print_endline;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 89950138 |}]
;;
