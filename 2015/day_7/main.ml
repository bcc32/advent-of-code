open! Core
open! Async
open! Import

module Def = struct
  type prim =
    | Alias of string
    | Const of int

  type t =
    | Prim of prim
    | AND of prim * prim
    | OR of prim * prim
    | NOT of prim
    | LSHIFT of prim * prim
    | RSHIFT of prim * prim
  [@@deriving variants]

  let of_string =
    let parser =
      let open Angstrom in
      let prim_parser =
        (let+ digits = take_while1 Char.is_digit in
         Const (Int.of_string digits))
        <|> let+ name = take_while1 Char.is_alpha in
            Alias name
      in
      let unop =
        let+ prim = string "NOT " *> prim_parser in
        NOT prim
      in
      let binop_name =
        string "AND" *> return and_
        <|> string "OR" *> return or_
        <|> string "LSHIFT" *> return lshift
        <|> string "RSHIFT" *> return rshift
      in
      let binop =
        map3
          prim_parser
          (char ' ' *> binop_name <* char ' ')
          prim_parser
          ~f:(fun lhs op_name rhs -> op_name lhs rhs)
      in
      let def = binop <|> unop <|> (prim_parser >>| prim) in
      map2
        def
        (string " -> "
         *> let+ name = take_while1 Char.is_alpha in
            name)
        ~f:(fun def name -> name, def)
    in
    fun string ->
      Angstrom.parse_string ~consume:All parser string
      |> Result.map_error ~f:Error.of_string
      |> ok_exn
  ;;

  let rec deps t =
    match (t : t) with
    | Prim (Alias x) -> [ x ]
    | Prim (Const _) -> []
    | AND (x, y) | OR (x, y) | LSHIFT (x, y) | RSHIFT (x, y) ->
      deps (Prim x) @ deps (Prim y)
    | NOT x -> deps (Prim x)
  ;;

  let rec eval_exn t values =
    match (t : t) with
    | Prim (Alias x) -> Hashtbl.find_exn values x
    | Prim (Const i) -> i
    | AND (x, y) -> eval_exn (Prim x) values land eval_exn (Prim y) values
    | OR (x, y) -> eval_exn (Prim x) values lor eval_exn (Prim y) values
    | LSHIFT (x, y) -> eval_exn (Prim x) values lsl eval_exn (Prim y) values
    | RSHIFT (x, y) -> eval_exn (Prim x) values lsr eval_exn (Prim y) values
    | NOT x -> lnot (eval_exn (Prim x) values)
  ;;
end

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| String.split_lines
    >>| List.map ~f:Def.of_string
    >>| String.Table.of_alist_exn)
;;

let iterate values defns =
  let ok = ref false in
  Hashtbl.iteri defns ~f:(fun ~key ~data ->
    match Hashtbl.find values key with
    | Some _ -> ()
    | None ->
      let deps = Def.deps data in
      if List.for_all deps ~f:(Hashtbl.mem values)
      then (
        ok := true;
        Hashtbl.set values ~key ~data:(Def.eval_exn data values)));
  !ok
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let values = String.Table.create () in
  while iterate values input do
    ()
  done;
  print_s [%sexp (Hashtbl.find_exn values "a" : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 3176 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let values = String.Table.create () in
  while iterate values input do
    ()
  done;
  let x = Hashtbl.find_exn values "a" in
  Hashtbl.clear values;
  Hashtbl.set values ~key:"b" ~data:x;
  while iterate values input do
    ()
  done;
  print_s [%sexp (Hashtbl.find_exn values "a" : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 14710 |}];
  return ()
;;
