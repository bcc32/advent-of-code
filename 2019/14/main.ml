open! Core
open! Async
open! Import

let debug = false

module Reagent = struct
  type t =
    { amt : int
    ; name : string
    }
  [@@deriving compare, hash, sexp_of]
end

module Reaction = struct
  type t =
    { inputs : Reagent.t list
    ; output : Reagent.t
    }
end

let parse_line =
  let rex =
    let open Re in
    compile (seq [ group (rep1 digit); str " "; group (rep1 upper) ])
  in
  fun s ->
    let reagents =
      Re.all rex s
      |> List.map ~f:(fun group ->
        let amt = Re.Group.get group 1 |> Int.of_string in
        let name = Re.Group.get group 2 in
        { Reagent.amt; name })
    in
    let inputs = List.drop_last_exn reagents in
    let output = List.last_exn reagents in
    { Reaction.inputs; output }
;;

let input () =
  let%map lines = Reader.file_lines "input" in
  lines
  |> List.map ~f:parse_line
  |> String.Table.create_with_key_exn ~get_key:(fun (reaction : Reaction.t) ->
    reaction.output.name)
;;

let ore_cost ~how_much_fuel ~(how_to_make : Reaction.t String.Table.t) =
  let nodes =
    Topological_sort.sort
      (module String)
      (Hashtbl.keys how_to_make)
      (Hashtbl.data how_to_make
       |> List.concat_map ~f:(fun reaction ->
         List.map reaction.inputs ~f:(fun r ->
           { Topological_sort.Edge.from = reaction.output.name; to_ = r.name })))
    |> ok_exn
  in
  let want_to_make = String.Table.of_alist_exn [ "FUEL", how_much_fuel ] in
  List.iter
    (List.filter nodes ~f:(function
       | "ORE" -> false
       | _ -> true))
    ~f:(fun need ->
      if debug then Debug.eprint_s [%message (want_to_make : (string, int) Hashtbl.t)];
      let need_amt = Hashtbl.find_and_remove want_to_make need |> uw in
      let rxn = Hashtbl.find_exn how_to_make need in
      let how_many_rxn =
        Int.round_up need_amt ~to_multiple_of:rxn.output.amt / rxn.output.amt
      in
      rxn.inputs
      |> List.iter ~f:(fun { amt; name } ->
        Hashtbl.incr want_to_make name ~by:(amt * how_many_rxn)));
  Hashtbl.find_exn want_to_make "ORE"
;;

let a () =
  let%bind how_to_make = input () in
  ore_cost ~how_much_fuel:1 ~how_to_make |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 654909 |}]
;;

let how_much_fuel_can_we_make ~max_ore ~how_to_make =
  let rec loop1 power_of_two =
    if ore_cost ~how_much_fuel:power_of_two ~how_to_make > max_ore
    then power_of_two
    else loop1 (power_of_two * 2)
  in
  let ceil_pow2_loop1 = loop1 1 in
  let rec bisect ubound pow2 =
    (* The desired value is in [ubound - pow2, ubound) *)
    if pow2 = 1
    then ubound - 1
    else (
      let midpoint = ubound - (pow2 / 2) in
      if ore_cost ~how_much_fuel:midpoint ~how_to_make > max_ore
      then bisect midpoint (pow2 / 2)
      else bisect ubound (pow2 / 2))
  in
  bisect ceil_pow2_loop1 ceil_pow2_loop1
;;

let b () =
  let%bind how_to_make = input () in
  let max_fuel = how_much_fuel_can_we_make ~max_ore:1_000_000_000_000 ~how_to_make in
  printf "%d\n" max_fuel;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 2876992 |}]
;;
