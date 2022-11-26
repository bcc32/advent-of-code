open! Core
open! Async
open! Import

let input = Lazy_deferred.create (fun () -> Reader.file_lines "input.txt")

let count_vowels =
  String.count ~f:(function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false)
;;

let contains_double_letter string =
  String.to_list string
  |> List.find_consecutive_duplicate ~equal:Char.equal
  |> Option.is_some
;;

let forbidden_strings = [ "ab"; "cd"; "pq"; "xy" ]

let is_nice string =
  count_vowels string >= 3
  && contains_double_letter string
  && not
       (List.exists forbidden_strings ~f:(fun substring ->
          String.is_substring string ~substring))
;;

let a () =
  let%bind strings = Lazy_deferred.force_exn input in
  let nice = List.count strings ~f:is_nice in
  print_s [%sexp (nice : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 258 |}];
  return ()
;;

let digraphs =
  [%all: Char.t * Char.t]
  |> List.filter ~f:(fun (a, b) -> Char.is_lowercase a && Char.is_lowercase b)
  |> List.map ~f:(fun (a, b) -> String.of_char_list [ a; b ])
;;

let is_nice string =
  let has_digraph_repeated =
    List.exists digraphs ~f:(fun digraph ->
      let pattern = String.Search_pattern.create digraph in
      match String.Search_pattern.index pattern ~in_:string with
      | None -> false
      | Some i ->
        String.Search_pattern.index ~pos:(i + 2) pattern ~in_:string |> Option.is_some)
  in
  let has_sandwich =
    List.range 0 (String.length string - 2)
    |> List.exists ~f:(fun i -> Char.( = ) string.[i] string.[i + 2])
  in
  has_digraph_repeated && has_sandwich
;;

let b () =
  let%bind strings = Lazy_deferred.force_exn input in
  let nice = List.count strings ~f:is_nice in
  print_s [%sexp (nice : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 53 |}];
  return ()
;;
