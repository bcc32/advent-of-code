open! Core
open! Async
open! Import

let main () =
  let%bind claims =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Claim.of_string |> Pipe.to_list)
  in
  let claims_by_square = Hashtbl.create (module Tuple.Hashable_t (Int) (Int)) in
  List.iter claims ~f:(fun claim ->
    Claim.iter claim ~f:(fun x y ->
      Hashtbl.add_multi claims_by_square ~key:(x, y) ~data:claim.id));
  let free_claims = List.map claims ~f:(fun c -> c.id) |> Int.Hash_set.of_list in
  Hashtbl.iter claims_by_square ~f:(function
    | [] -> assert false
    | [ _ ] -> () (* non overlapping *)
    | _ :: _ :: _ as claims -> List.iter claims ~f:(Hash_set.remove free_claims));
  match Hash_set.to_list free_claims with
  | [] -> failwith "no claims"
  | [ chosen ] ->
    printf "%d\n" chosen;
    return ()
  | claims -> raise_s [%message "wrong number of claims" (claims : int list)]
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 504 |}]
;;
