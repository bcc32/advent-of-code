[@@@warning "-partial-match"]

open! Core

let pat = Re.(compile (seq [ opt (char '-'); rep1 digit ]))

let input =
  In_channel.read_lines "aoc.in"
  |> List.map ~f:(fun line ->
    match Re.all pat line |> List.map ~f:(fun g -> Re.Group.get g 0 |> Int.of_string) with
    | [ px; py; pz; vx; vy; vz ] -> px, py, pz, vx, vy, vz)
;;

let make_rules (px, py, pz, vx, vy, vz) ~context ~int ~x0 ~y0 ~z0 ~vx0 ~vy0 ~vz0 =
  let num x = Z3.Expr.mk_numeral_int context x int in
  let px, py, pz, vx, vy, vz = num px, num py, num pz, num vx, num vy, num vz in
  let t = Z3.Expr.mk_fresh_const context "t" int in
  let ( + ) a b = Z3.Arithmetic.mk_add context [ a; b ] in
  let ( * ) a b = Z3.Arithmetic.mk_mul context [ a; b ] in
  [ Z3.Boolean.mk_eq context (x0 + (t * vx0)) (px + (t * vx))
  ; Z3.Boolean.mk_eq context (y0 + (t * vy0)) (py + (t * vy))
  ; Z3.Boolean.mk_eq context (z0 + (t * vz0)) (pz + (t * vz))
  ; Z3.Arithmetic.mk_ge context t (num 0)
  ]
;;

let main () =
  let context = Z3.mk_context [] in
  let solver = Z3.Solver.mk_simple_solver context in
  let int = Z3.Arithmetic.Real.mk_sort context in
  let x0, y0, z0, vx0, vy0, vz0 =
    ( Z3.Expr.mk_const_s context "x0" int
    , Z3.Expr.mk_const_s context "y0" int
    , Z3.Expr.mk_const_s context "z0" int
    , Z3.Expr.mk_const_s context "vx0" int
    , Z3.Expr.mk_const_s context "vy0" int
    , Z3.Expr.mk_const_s context "vz0" int )
  in
  let rules =
    List.concat_map
      (List.take input 3)
      ~f:(make_rules ~context ~int ~x0 ~y0 ~z0 ~vx0 ~vy0 ~vz0)
  in
  Z3.Solver.add solver rules;
  match Z3.Solver.check solver [] with
  | UNSATISFIABLE | UNKNOWN -> raise_s [%message "no solution"]
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Solver.get_model solver |> Option.value_exn in
    print_endline (Z3.Model.to_string model);
    let ans =
      let get var =
        Z3.Model.get_const_interp_e model var
        |> Option.value_exn
        |> Z3.Arithmetic.Real.numeral_to_string
        |> Float.of_string
      in
      get x0 +. get y0 +. get z0
    in
    print_s [%sexp (ans : float)]
;;

let () = main ()
