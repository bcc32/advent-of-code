open! Core
open! Async
open! Import

module Stats = struct
  type t =
    { mutable hp : int
    ; dmg : int
    ; armor : int
    }
  [@@deriving fields, sexp_of]

  let create = Fields.create
  let copy t = { t with hp = t.hp }

  let do_attack t1 ~to_:t2 =
    let dmg = Int.max 1 (t1.dmg - t2.armor) in
    t2.hp <- t2.hp - dmg
  ;;

  let is_dead t = t.hp <= 0
end

let parse_boss =
  let get_int s =
    let pat =
      let open Re in
      compile (rep1 digit)
    in
    Re.exec pat s |> Fn.flip Re.Group.get 0 |> Int.of_string
  in
  fun lines ->
    match lines with
    | [ hp; dmg; armor ] ->
      let hp = get_int hp in
      let dmg = get_int dmg in
      let armor = get_int armor in
      Stats.create ~hp ~dmg ~armor
    | _ -> failwith "parse_boss"
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| parse_boss)
;;

let simulate ~player_stats ~boss_stats =
  let player_stats = Stats.copy player_stats in
  let boss_stats = Stats.copy boss_stats in
  let rec player_attack () =
    Stats.do_attack player_stats ~to_:boss_stats;
    if Stats.is_dead boss_stats then `Player_win else boss_attack ()
  and boss_attack () =
    Stats.do_attack boss_stats ~to_:player_stats;
    if Stats.is_dead player_stats then `Boss_win else player_attack ()
  in
  player_attack ()
;;

let weapons = [ 8, 4; 10, 5; 25, 6; 40, 7; 74, 8 ]
let armors = [ 0, 0; 13, 1; 31, 2; 53, 3; 75, 4; 102, 5 ]

let rings =
  [ 0, 0, 0; 0, 0, 0; 25, 1, 0; 50, 2, 0; 100, 3, 0; 20, 0, 1; 40, 0, 2; 80, 0, 3 ]
;;

let iter_combo2 list ~f =
  let rec loop list =
    match list with
    | [] | [ _ ] -> ()
    | x :: (_ :: _ as tl) ->
      List.iter tl ~f:(fun y -> f x y);
      loop tl
  in
  loop list
;;

let iter_equipment ~f =
  List.iter weapons ~f:(fun (weapon_cost, weapon_damage) ->
    List.iter armors ~f:(fun (armor_cost, armor_armor) ->
      iter_combo2
        rings
        ~f:(fun
             (ring1_cost, ring1_damage, ring1_armor)
             (ring2_cost, ring2_damage, ring2_armor)
             ->
               let cost = weapon_cost + armor_cost + ring1_cost + ring2_cost in
               let dmg = weapon_damage + ring1_damage + ring2_damage in
               let armor = armor_armor + ring1_armor + ring2_armor in
               f ~cost ~stats:(Stats.create ~hp:100 ~dmg ~armor))))
;;

let a () =
  let%bind boss_stats = Lazy_deferred.force_exn input in
  let player_min_cost = ref Int.max_value in
  iter_equipment ~f:(fun ~cost ~stats:player_stats ->
    match simulate ~player_stats ~boss_stats with
    | `Player_win -> player_min_cost := Int.min !player_min_cost cost
    | `Boss_win -> ());
  print_s [%sexp (!player_min_cost : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 91 |}];
  return ()
;;

let b () =
  let%bind boss_stats = Lazy_deferred.force_exn input in
  let lose_max_cost = ref Int.min_value in
  iter_equipment ~f:(fun ~cost ~stats:player_stats ->
    match simulate ~player_stats ~boss_stats with
    | `Player_win -> ()
    | `Boss_win -> lose_max_cost := Int.max !lose_max_cost cost);
  print_s [%sexp (!lose_max_cost : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 158 |}];
  return ()
;;
