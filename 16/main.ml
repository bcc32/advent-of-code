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

let do_phase input =
  Array.mapi input ~f:(fun i _ ->
    let sum = ref 0 in
    Array.iteri input ~f:(fun j x -> sum := !sum + (x * pattern i j));
    Int.abs (!sum mod 10))
;;

let a () =
  let%bind input = input () in
  let output = Fn.apply_n_times ~n:100 do_phase input in
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

let b () =
  let%bind _input = input () in
  failwith "unimplemented"
;;

let%expect_test "b" =
  let%bind () = show_raise_async b in
  [%expect {| (raised (Failure unimplemented)) |}]
;;
