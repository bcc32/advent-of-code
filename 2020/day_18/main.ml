open! Core
open! Async
open! Import

module Op = struct
  type t =
    | Plus
    | Times
  [@@deriving sexp_of]

  let apply t x y =
    match t with
    | Plus -> x + y
    | Times -> x * y
  ;;
end

module Expr = struct
  type t =
    | Literal of int
    | Combination of t * (Op.t * t) list
  [@@deriving sexp_of]

  let rec evaluate t =
    match t with
    | Literal n -> n
    | Combination (hd, ops) ->
      List.fold ops ~init:(evaluate hd) ~f:(fun x (op, y) -> Op.apply op x (evaluate y))
  ;;

  let rec evaluate' t =
    match t with
    | Literal n -> n
    | Combination (hd, ops) ->
      (match
         List.findi ops ~f:(fun _ -> function
           | Plus, _ -> true
           | Times, _ -> false)
       with
       | None ->
         List.fold ops ~init:(evaluate' hd) ~f:(fun x (op, y) ->
           Op.apply op x (evaluate' y))
       | Some (i, _) ->
         let nums = hd :: List.map ops ~f:snd in
         let operators = List.map ops ~f:fst in
         (* 0 * 1 + 2 * 3 * 4

            i = 1

            0 * (1 + 2) * 3 * 4 *)
         let before_nums = List.take nums i in
         let after_nums = List.drop nums (i + 2) in
         let x = List.nth_exn nums i |> evaluate' in
         let y = List.nth_exn nums (i + 1) |> evaluate' in
         let before_ops = List.take operators i in
         let after_ops = List.drop operators (i + 1) in
         let new_nums = before_nums @ [ Literal (x + y) ] @ after_nums in
         let hd = List.hd_exn new_nums in
         let tl = List.tl_exn new_nums in
         evaluate' (Combination (hd, List.zip_exn (before_ops @ after_ops) tl)))
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Expr.t list [@@deriving sexp_of]

  let parse =
    let parser =
      let open Angstrom in
      let literal =
        (let+ digits = take_while1 Char.is_digit in
         digits |> Int.of_string |> Expr.Literal)
        <?> "literal"
      in
      let op =
        (let+ _ = char '+' in
         Op.Plus)
        <|> (let+ _ = char '*' in
             Op.Times)
        <?> "op"
      in
      fix (fun (expr : Expr.t Angstrom.t) ->
        let atom =
          let parens =
            let* _ = char '(' in
            let* t = expr in
            let* _ = char ')' in
            return t
          in
          literal <|> parens <?> "atom"
        in
        let combination =
          (let* head = atom in
           let* ops_with_tails =
             many
               (let* () = skip_while Char.is_whitespace in
                let* op = op in
                let* () = skip_while Char.is_whitespace in
                let* atom = atom in
                return (op, atom))
           in
           return (Expr.Combination (head, ops_with_tails)))
          <?> "combination"
        in
        combination <|> atom)
    in
    fun input ->
      List.map (lines input) ~f:(fun line ->
        Angstrom.parse_string ~consume:All parser line
        |> Result.map_error ~f:Error.of_string
        |> ok_exn)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let a () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  List.sum (module Int) input ~f:Expr.evaluate |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 21022630974613 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  List.sum (module Int) input ~f:Expr.evaluate' |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 169899524778212 |}];
  return ()
;;
