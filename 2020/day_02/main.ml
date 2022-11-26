open! Core
open! Async
open! Import

let parse_line line =
  let re =
    let open Re in
    compile
      (seq
         [ group (rep1 digit)
         ; char '-'
         ; group (rep1 digit)
         ; char ' '
         ; group any
         ; str ": "
         ; group (rep1 any)
         ])
  in
  let group = Re.exec re line in
  let min = Re.Group.get group 1 |> Int.of_string in
  let max = Re.Group.get group 2 |> Int.of_string in
  let char = Re.Group.get group 3 |> Fn.flip String.get 0 in
  let passwd = Re.Group.get group 4 in
  min, max, char, passwd
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| List.map ~f:parse_line)
;;

let is_valid ~min ~max ~char ~passwd =
  Int.between (String.count passwd ~f:(Char.equal char)) ~low:min ~high:max
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  List.count input ~f:(fun (min, max, char, passwd) -> is_valid ~min ~max ~char ~passwd)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 458 |}];
  return ()
;;

let is_valid ~min ~max ~char ~passwd =
  Bool.( <> ) (Char.equal passwd.[min - 1] char) (Char.equal passwd.[max - 1] char)
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  List.count input ~f:(fun (min, max, char, passwd) -> is_valid ~min ~max ~char ~passwd)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 342 |}];
  return ()
;;
