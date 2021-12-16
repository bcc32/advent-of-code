open! Core
open! Async
open! Import

let hamming_dist a b =
  let count = ref 0 in
  for i = 0 to String.length a - 1 do
    if Char.O.(a.[i] <> b.[i]) then incr count
  done;
  !count
;;

let common a b =
  a
  |> String.to_list
  |> List.filteri ~f:(fun i c -> Char.O.(c = b.[i]))
  |> String.of_char_list
;;

let main () =
  let%bind lines =
    Reader.with_file "input" ~f:(fun r -> Reader.lines r |> Pipe.to_list)
  in
  (try
     List.iter lines ~f:(fun x ->
       List.iter lines ~f:(fun y ->
         if hamming_dist x y = 1
         then (
           printf "%s\n" (common x y);
           raise Exit)))
   with
   | Exit -> ());
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| mxhwoglxgeauywfkztndcvjqr |}]
;;
