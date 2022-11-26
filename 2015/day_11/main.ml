open! Core
open! Async
open! Import

let increment b =
  let module String = Bytes in
  let rec loop pos =
    if Char.equal b.[pos] 'z'
    then (
      b.[pos] <- 'a';
      loop (pos - 1))
    else (
      b.[pos] <- Char.of_int_exn (Char.to_int b.[pos] + 1);
      while Core.String.mem "iol" b.[pos] do
        b.[pos] <- Char.of_int_exn (Char.to_int b.[pos] + 1)
      done)
  in
  loop (Bytes.length b - 1)
;;

let straights =
  Char.all
  |> List.filter ~f:(Char.between ~low:'a' ~high:'x')
  |> List.map ~f:(fun c ->
    let c = Char.to_int c in
    [ c; c + 1; c + 2 ] |> List.map ~f:Char.of_int_exn |> String.of_char_list)
;;

let is_valid s =
  let re =
    let open Re in
    compile
      (let repeat_letter =
         Char.all
         |> List.filter ~f:Char.is_lowercase
         |> List.map ~f:(fun c -> seq [ char c; char c ])
         |> alt
       in
       seq [ repeat_letter; rep any; repeat_letter ])
  in
  List.exists straights ~f:(fun straight -> String.is_substring s ~substring:straight)
  && (not (String.exists "iol" ~f:(fun bad_char -> String.mem s bad_char)))
  && Re.execp re s
;;

let find start =
  let start = Bytes.of_string start in
  increment start;
  while
    not (is_valid (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:start))
  do
    increment start
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:start
;;

let input =
  Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| String.rstrip)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = find input in
  print_s [%sexp (ans : string)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| cqjxxyzz |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = input |> find |> find in
  print_s [%sexp (ans : string)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| cqkaabcc |}];
  return ()
;;
