open! Core
open! Async
open! Import

module Food = struct
  type t =
    { ingredients : string list
    ; known_allergens : string list
    }
  [@@deriving fields, sexp_of]
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Food.t list [@@deriving sexp_of]

  let parse input : t =
    input
    |> lines
    |> List.map ~f:(fun line ->
      let re =
        let open Re in
        compile
          (seq
             [ group (seq [ rep1 wordc; rep (seq [ char ' '; rep1 wordc ]) ])
             ; str " (contains "
             ; group (seq [ rep1 wordc; rep (seq [ str ", "; rep1 wordc ]) ])
             ; str ")"
             ])
      in
      let g = Re.exec re line in
      let ingredients = Re.Group.get g 1 |> String.split ~on:' ' in
      let allergens = Re.Group.get g 2 |> words ~sep:", " in
      ({ ingredients; known_allergens = allergens } : Food.t))
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let deduce foods =
  (* Map allergen name to lists of set of ingredients in foods in which allergen
     appears. *)
  let all_allergens =
    foods
    |> List.concat_map ~f:Food.known_allergens
    |> String.Set.of_list
    |> Set.to_map ~f:(fun allergen ->
      List.filter_map foods ~f:(fun food ->
        if List.mem food.known_allergens allergen ~equal:String.equal
        then Some (String.Set.of_list food.ingredients)
        else None))
    |> Map.to_alist
    |> String.Table.of_alist_exn
  in
  let deduced_allergens_to_ingredients = String.Table.create () in
  let deduced_ingredients_to_allergens = String.Table.create () in
  while
    Hashtbl.existsi all_allergens ~f:(fun ~key:allergen ~data:ingredients_lists ->
      let result =
        if Hashtbl.mem deduced_allergens_to_ingredients allergen
        then `No_op
        else (
          let ingredients_lists =
            List.map
              ingredients_lists
              ~f:(Set.filter ~f:(not << Hashtbl.mem deduced_ingredients_to_allergens))
          in
          let set_of_ingredients_in_common =
            List.reduce_exn ingredients_lists ~f:Set.inter
          in
          if Set.length set_of_ingredients_in_common = 1
          then (
            let ingredient = Set.choose_exn set_of_ingredients_in_common in
            Hashtbl.add_exn
              deduced_allergens_to_ingredients
              ~key:allergen
              ~data:ingredient;
            Hashtbl.add_exn
              deduced_ingredients_to_allergens
              ~key:ingredient
              ~data:allergen;
            `Made_deduction)
          else `No_op)
      in
      match result with
      | `Made_deduction -> true
      | `No_op -> false)
  do
    ()
  done;
  let ingredients_definitely_not_containing_allergens =
    List.map foods ~f:(fun food ->
      if List.for_all food.known_allergens ~f:(fun allergen ->
        match Hashtbl.find deduced_allergens_to_ingredients allergen with
        | Some ingredient -> List.mem food.ingredients ingredient ~equal:String.equal
        | None -> false)
      then
        Set.diff
          (String.Set.of_list food.ingredients)
          (deduced_ingredients_to_allergens |> Hashtbl.keys |> String.Set.of_list)
      else String.Set.empty)
    |> String.Set.union_list
  in
  ( ingredients_definitely_not_containing_allergens
  , deduced_allergens_to_ingredients
  , deduced_ingredients_to_allergens )
;;

let a () =
  let%bind foods = Lazy_deferred.force_exn Input.t in
  let ingredients_definitely_not_containing_allergens, _, _ = deduce foods in
  Set.sum
    (module Int)
    ingredients_definitely_not_containing_allergens
    ~f:(fun ing ->
      List.count foods ~f:(fun food -> List.mem food.ingredients ing ~equal:String.equal))
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2627 |}];
  return ()
;;

let b () =
  let%bind foods = Lazy_deferred.force_exn Input.t in
  let ( _ingredients_definitely_not_containing_allergens
      , allergen_to_ingredient
      , _ingredient_to_allergen )
    =
    deduce foods
  in
  let allergen_to_ingredient =
    String.Map.of_alist_exn (Hashtbl.to_alist allergen_to_ingredient)
  in
  Map.data allergen_to_ingredient |> String.concat ~sep:"," |> print_endline;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| hn,dgsdtj,kpksf,sjcvsr,bstzgn,kmmqmv,vkdxfj,bsfqgb |}];
  return ()
;;
