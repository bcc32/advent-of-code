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

type rxn =
  { inputs : Reagent.t list
  ; output : Reagent.t
  }

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
    let inputs = List.drop_last reagents |> uw in
    let output = List.last_exn reagents in
    { inputs; output }
;;

let input () =
  let%map lines = Reader.file_lines "input" in
  let ways_to_make = String.Table.create () in
  let reactions = List.map lines ~f:parse_line in
  List.iter reactions ~f:(fun ({ inputs = _; output } as rxn) ->
    Hashtbl.add_multi ways_to_make ~key:output.name ~data:rxn);
  reactions, ways_to_make
;;

let ore_to_make ~how_much_fuel =
  let%bind _reactions, ways_to_make = input () in
  let nodes =
    Topological_sort.sort
      (module String)
      (Hashtbl.keys ways_to_make)
      (Hashtbl.to_alist ways_to_make
       |> List.concat_map ~f:(fun (output, reactions) ->
         List.concat_map reactions ~f:(fun reaction ->
           List.map reaction.inputs ~f:(fun r ->
             { Topological_sort.Edge.from = output; to_ = r.name }))))
    |> ok_exn
  in
  let want_to_make = String.Table.of_alist_exn [ "FUEL", how_much_fuel ] in
  List.iter
    (List.filter nodes ~f:(function
       | "ORE" -> false
       | _ -> true))
    ~f:(fun need ->
      if debug then Debug.eprint_s [%message (want_to_make : (string, int) Hashtbl.t)];
      let need_amt = Hashtbl.find_exn want_to_make need in
      let rxn = Hashtbl.find_multi ways_to_make need |> List.hd_exn in
      let how_many_rxn =
        Int.round_up need_amt ~to_multiple_of:rxn.output.amt / rxn.output.amt
      in
      rxn.inputs
      |> List.iter ~f:(fun { amt; name } ->
        Hashtbl.update want_to_make name ~f:(fun current_amt ->
          Option.value current_amt ~default:0 + (amt * how_many_rxn)));
      Hashtbl.remove want_to_make need);
  return (Hashtbl.data want_to_make |> List.hd_exn)
;;

let a () =
  let%bind ore_to_make = ore_to_make ~how_much_fuel:1 in
  printf "%d\n" ore_to_make;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 654909 |}]
;;

(* let how_much_fuel_can_we_make ~max_ore =
 *   let rec loop how_much_fuel =
 *   in
 *   loop 1
 *   let%bind ore_to_make = ore_to_make ~how_much_fuel:1 in
 *   printf "%d\n" ore_to_make;
 *   return ()
 * ;; *)

let b () =
  let%bind ore_to_make = ore_to_make ~how_much_fuel:2876992 in
  printf "%d\n" ore_to_make;
  print_s
    [%sexp (Int.compare ore_to_make 1_000_000_000_000 |> Ordering.of_int : Ordering.t)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {|
    999999895242
    Less |}]
;;
