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
    let [ field_defs; my_ticket; nearby_tickets ] = paragraphs (lines input) in
    let fields : Valid_values.t Map.M(String).t =
      List.map field_defs ~f:(fun str ->
        let (name :: ranges) = Re.Pcre.split str ~rex:(Re.Pcre.regexp ": | or ") in
        ( name
        , List.map ranges ~f:(fun x ->
            let [ left; right ] = String.split x ~on:'-' in
            stoi left, stoi right) ))
      |> Map.of_alist_exn (module String)
    in
    let my_ticket = List.nth_exn my_ticket 1 |> Ticket.of_string in
    let nearby_tickets = List.tl_exn nearby_tickets |> List.map ~f:Ticket.of_string in
    { fields; my_ticket; nearby_tickets }
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

let is_valid ticket ~permutation ~fields =
  List.for_all2_exn ticket permutation ~f:(fun field_value field_name ->
    let ranges = Map.find_exn fields field_name in
    List.exists ranges ~f:(fun (low, high) -> Int.between ~low ~high field_value))
;;

let possible_fields_in_position tickets i ~fields =
  List.filter (Map.to_alist fields) ~f:(fun (field_name, ranges) ->
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
  let possible_fields_in_position =
    List.init (List.length input.my_ticket) ~f:(fun i ->
      possible_fields_in_position tickets_to_check i ~fields:input.fields)
    |> ref
  in
  while !possible_fields_in_position |> List.exists ~f:(fun l -> List.length l > 1) do
    let fixed_in_position =
      List.filter_map !possible_fields_in_position ~f:(fun fields ->
        if List.length fields = 1 then Some (List.hd_exn fields) else None)
    in
    possible_fields_in_position
    := List.map !possible_fields_in_position ~f:(fun list ->
      if List.length list = 1
      then list
      else
        List.filter list ~f:(fun field ->
          not (List.mem fixed_in_position field ~equal:String.equal)))
  done;
  let correct_permutation = List.map !possible_fields_in_position ~f:List.hd_exn in
  List.fold2_exn
    ~init:1
    input.my_ticket
    correct_permutation
    ~f:(fun accum field_value field_name ->
      if String.is_prefix field_name ~prefix:"departure"
      then accum * field_value
      else accum)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 453459307723 |}] in
  return ()
;;
