open! Core
open! Async
open! Import

let count_letters s =
  let tbl = Char.Table.create () in
  String.iter s ~f:(Hashtbl.incr tbl);
  Hashtbl.data tbl
;;

let main () =
  let%bind two, three =
    Reader.with_file "input" ~f:(fun r ->
      Reader.lines r
      |> Pipe.fold_without_pushback ~init:(0, 0) ~f:(fun (two, three) line ->
        let counts = count_letters line in
        let two = two + Bool.to_int (List.mem counts 2 ~equal:Int.equal) in
        let three = three + Bool.to_int (List.mem counts 3 ~equal:Int.equal) in
        two, three))
  in
  two * three |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 6474 |}];
  return ()
;;
