open! Core
open! Async
open! Import

module Valid_values = struct
  type t = (int * int) list [@@deriving sexp_of]
end

module Ticket = struct
  type t = int list [@@deriving sexp_of]

  let of_string = String.split ~on:',' >> List.map ~f:Int.of_string
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t =
    { fields : Valid_values.t Map.M(String).t
    ; my_ticket : Ticket.t
    ; nearby_tickets : Ticket.t list
    }
  [@@deriving sexp_of]

  let parse input : t =
    match paragraphs (lines input) with
    | [ field_defs; my_ticket; nearby_tickets ] ->
      let fields : Valid_values.t Map.M(String).t =
        List.map field_defs ~f:(fun str ->
          match Re.Pcre.(split str ~rex:(regexp ": | or ")) with
          | name :: ranges ->
            ( name
            , List.map ranges ~f:(fun x ->
                let left, right = String.lsplit2_exn x ~on:'-' in
                stoi left, stoi right) )
          | _ -> assert false)
        |> Map.of_alist_exn (module String)
      in
      let my_ticket = List.nth_exn my_ticket 1 |> Ticket.of_string in
      let nearby_tickets = List.tl_exn nearby_tickets |> List.map ~f:Ticket.of_string in
      { fields; my_ticket; nearby_tickets }
    | _ -> assert false
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let find_invalid_field_value ticket ~fields =
  List.find ticket ~f:(fun field_value ->
    Map.data fields
    |> List.for_all ~f:(fun ranges ->
      List.for_all ranges ~f:(fun (low, high) ->
        not (Int.between field_value ~low ~high))))
;;

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let error_rate = ref 0 in
  List.iter input.nearby_tickets ~f:(fun ticket ->
    match find_invalid_field_value ticket ~fields:input.fields with
    | None -> ()
    | Some x -> error_rate := !error_rate + x);
  print_s [%sexp (!error_rate : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 23954 |}] in
  return ()
;;

let possible_fields_in_position tickets i ~fields =
  List.filter (Map.to_alist fields) ~f:(fun (_, ranges) ->
    List.for_all tickets ~f:(fun ticket ->
      let field_value = List.nth_exn ticket i in
      List.exists ranges ~f:(fun (low, high) -> Int.between ~low ~high field_value)))
  |> List.map ~f:fst
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  let valid_nearby_tickets =
    List.filter input.nearby_tickets ~f:(fun ticket ->
      match find_invalid_field_value ticket ~fields:input.fields with
      | None -> true
      | Some _ -> false)
  in
  let tickets_to_check = input.my_ticket :: valid_nearby_tickets in
  let correct_permutation = Option_array.create ~len:(List.length input.my_ticket) in
  let possible_fields_in_position =
    Array.init (List.length input.my_ticket) ~f:(fun i ->
      possible_fields_in_position tickets_to_check i ~fields:input.fields)
  in
  while possible_fields_in_position |> Array.exists ~f:(not << List.is_empty) do
    let i, fixed_in_position =
      Array.find_mapi possible_fields_in_position ~f:(fun i fields ->
        if List.length fields = 1 then Some (i, List.hd_exn fields) else None)
      |> Option.value_exn
    in
    Option_array.set_some correct_permutation i fixed_in_position;
    Array.map_inplace
      possible_fields_in_position
      ~f:(List.filter ~f:(String.( <> ) fixed_in_position))
  done;
  let product = ref 1 in
  for i = 0 to Option_array.length correct_permutation - 1 do
    let field_name = Option_array.get_some_exn correct_permutation i in
    if String.is_prefix field_name ~prefix:"departure"
    then product := !product * List.nth_exn input.my_ticket i
  done;
  print_s [%sexp (!product : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 453459307723 |}] in
  return ()
;;
