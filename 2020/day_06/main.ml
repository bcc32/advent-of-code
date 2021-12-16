open! Core
open! Async
open! Import

module Input = struct
  module Person = struct
    type t = string [@@deriving sexp_of]

    let answers t = String.to_list t |> Set.of_list (module Char)
  end

  module Group = struct
    type t = Person.t list [@@deriving sexp_of]

    let union t = List.map t ~f:Person.answers |> Set.union_list (module Char)
    let inter t = List.map t ~f:Person.answers |> List.reduce_exn ~f:Set.inter
  end

  type t = Group.t list [@@deriving sexp_of]

  let parse input : t =
    input
    |> String.split_lines
    |> List.group ~break:(fun _ s -> String.is_empty s)
    |> List.map ~f:(List.filter ~f:(String.( <> ) ""))
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let count_answers group = Input.Group.union group |> Set.length

let a () =
  let%bind groups = Lazy_deferred.force_exn Input.t in
  List.sum (module Int) groups ~f:count_answers |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 6351 |}] in
  return ()
;;

let count_answers group = Input.Group.inter group |> Set.length

let b () =
  let%bind groups = Lazy_deferred.force_exn Input.t in
  List.sum (module Int) groups ~f:count_answers |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 3143 |}] in
  return ()
;;
