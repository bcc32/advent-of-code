open! Core
open! Async
open! Import

type present =
  { l : int
  ; w : int
  ; h : int
  }

let input () : present list Deferred.t =
  let%bind lines = Reader.file_lines "input.txt" in
  List.map lines ~f:(fun line ->
    match String.split line ~on:'x' with
    | [ l; w; h ] -> { l = Int.of_string l; w = Int.of_string w; h = Int.of_string h }
    | _ -> failwith "bad line")
  |> return
;;

let paper_needed { l; w; h } =
  let sides = [ l * w; w * h; l * h ] in
  (2 * List.sum (module Int) sides ~f:Fn.id)
  + Option.value_exn (List.min_elt ~compare:Int.compare sides)
;;

let a () =
  let%bind presents = input () in
  List.sum (module Int) presents ~f:(fun present -> paper_needed present)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 1606483 |}];
  return ()
;;

let ribbon { l; w; h } =
  let perimeters = [ (2 * l) + (2 * w); (2 * w) + (2 * h); (2 * l) + (2 * h) ] in
  let smallest_perimeter =
    List.min_elt perimeters ~compare:Int.compare |> Option.value_exn
  in
  let bow = l * w * h in
  smallest_perimeter + bow
;;

let b () =
  let%bind presents = input () in
  List.sum (module Int) presents ~f:ribbon |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 3842356 |}];
  return ()
;;
