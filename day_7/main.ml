open! Core
open! Async
open! Import

module Def = struct
  type prim =
    | Alias of string
    | Const of int

  and t =
    | Prim of prim
    | AND of prim * prim
    | OR of prim * prim
    | NOT of prim
    | LSHIFT of prim * prim
    | RSHIFT of prim * prim

  let of_string =
    let re =
      let open Re in
      compile
        (let id = rep1 wordc in
         let lhs =
           alt
             [ seq [ str "NOT"; str " "; group id ]
             ; seq
                 [ group id
                 ; str " "
                 ; group (alt [ str "AND"; str "OR" ])
                 ; str " "
                 ; group id
                 ]
             ; seq
                 [ group id
                 ; str " "
                 ; group (alt [ str "LSHIFT"; str "RSHIFT" ])
                 ; str " "
                 ; group id
                 ]
             ; group id
             ]
         in
         seq [ bol; lhs; str " -> "; group id; eol ])
    in
    fun string ->
      match Re.exec_opt re string with
      | None -> raise_s [%message "no match" (string : string)]
      | Some g ->
        let prim x =
          match Int.of_string x with
          | i -> Const i
          | exception _ -> Alias x
        in
        let def =
          if Re.Group.test g 1
          then NOT (prim (Re.Group.get g 1))
          else if Re.Group.test g 3
          then (
            let lhs = Re.Group.get g 2 in
            let op = Re.Group.get g 3 in
            let rhs = Re.Group.get g 4 in
            match op with
            | "AND" -> AND (prim lhs, prim rhs)
            | "OR" -> OR (prim lhs, prim rhs)
            | _ -> assert false)
          else if Re.Group.test g 5
          then (
            let lhs = Re.Group.get g 5 in
            let op = Re.Group.get g 6 in
            let rhs = Re.Group.get g 7 in
            match op with
            | "LSHIFT" -> LSHIFT (prim lhs, prim rhs)
            | "RSHIFT" -> RSHIFT (prim lhs, prim rhs)
            | _ -> assert false)
          else Prim (prim (Re.Group.get g 8))
        in
        let name = Re.Group.get g 9 in
        name, def
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
  let%bind () = [%expect {|
    3176 |}] in
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
  let%bind () = [%expect {| 14710 |}] in
  return ()
;;
