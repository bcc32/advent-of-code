open! Core
open! Async
open! Import

module Effect = struct
  type t =
    | Shield
    | Poison
    | Recharge
  [@@deriving compare, equal, hash, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

module Spell = struct
  type t =
    | Magic_missile
    | Drain
    | Shield
    | Poison
    | Recharge
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let all_with_cost =
    [ Magic_missile, 53; Drain, 73; Shield, 113; Poison, 173; Recharge, 229 ]
  ;;
end

module Stats = struct
  type t =
    { hp : int
    ; mp : int
    ; dmg : int
    ; effects : int Map.M(Effect).t (** data is ticks remaining *)
    }
  [@@deriving compare, equal, fields, hash, sexp_of]

  let create = Fields.create
  let armor t = if Map.mem t.effects Shield then 7 else 0

  let do_attack t1 ~to_:t2 =
    let dmg = Int.max 1 (t1.dmg - armor t2) in
    { t2 with hp = t2.hp - dmg }
  ;;

  let do_spell t1 ~to_:t2 ~(spell : Spell.t) =
    match spell with
    | Magic_missile ->
      if t1.mp >= 53
      then Some ({ t1 with mp = t1.mp - 53 }, { t2 with hp = t2.hp - 4 })
      else None
    | Drain ->
      if t1.mp >= 73
      then Some ({ t1 with mp = t1.mp - 73; hp = t1.hp + 2 }, { t2 with hp = t2.hp - 2 })
      else None
    | Shield ->
      if t1.mp >= 113
      then (
        match Map.add t1.effects ~key:Shield ~data:6 with
        | `Duplicate -> None
        | `Ok effects -> Some ({ t1 with effects; mp = t1.mp - 113 }, t2))
      else None
    | Poison ->
      if t1.mp >= 173
      then (
        match Map.add t2.effects ~key:Poison ~data:6 with
        | `Duplicate -> None
        | `Ok effects -> Some ({ t1 with mp = t1.mp - 173 }, { t2 with effects }))
      else None
    | Recharge ->
      if t1.mp >= 229
      then (
        match Map.add t1.effects ~key:Recharge ~data:5 with
        | `Duplicate -> None
        | `Ok effects -> Some ({ t1 with mp = t1.mp - 229; effects }, t2))
      else None
  ;;

  let is_dead t = t.hp <= 0

  let apply_effects t =
    Map.keys t.effects
    |> List.fold ~init:t ~f:(fun t effect ->
      let t =
        { t with
          effects =
            Map.change t.effects effect ~f:(function
              | None -> assert false
              | Some ticks_left -> if ticks_left = 1 then None else Some (ticks_left - 1))
        }
      in
      match effect with
      | Shield -> t
      | Poison -> { t with hp = t.hp - 3 }
      | Recharge -> { t with mp = t.mp + 101 })
  ;;
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
    | [ hp; dmg ] ->
      let hp = get_int hp in
      let dmg = get_int dmg in
      Stats.create ~hp ~mp:0 ~dmg ~effects:(Map.empty (module Effect))
    | _ -> failwith "parse_boss"
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| parse_boss)
;;

let min_mana_for_player_win ~player_stats ~boss_stats ~hard =
  let player_turn_neighbors ~(player_stats : Stats.t) ~boss_stats ~mana_spent ~spells =
    let player_stats =
      if hard then { player_stats with hp = player_stats.hp - 1 } else player_stats
    in
    let player_stats = Stats.apply_effects player_stats in
    let boss_stats = Stats.apply_effects boss_stats in
    if Stats.is_dead player_stats
    then []
    else
      List.filter_map Spell.all_with_cost ~f:(fun (spell, cost) ->
        let%map.Option player_stats, boss_stats =
          Stats.do_spell player_stats ~to_:boss_stats ~spell
        in
        `Boss_turn, player_stats, boss_stats, mana_spent + cost, spell :: spells)
  and boss_turn_neighbor ~player_stats ~boss_stats ~mana_spent ~spells =
    let player_stats = Stats.apply_effects player_stats in
    let boss_stats = Stats.apply_effects boss_stats in
    let player_stats =
      if not (Stats.is_dead boss_stats)
      then Stats.do_attack boss_stats ~to_:player_stats
      else player_stats
    in
    `Player_turn, player_stats, boss_stats, mana_spent, spells
  in
  let module State = struct
    type t = [ `Player_turn | `Boss_turn ] * Stats.t * Stats.t * int * Spell.t list
    [@@deriving compare, equal, hash, sexp_of]
  end
  in
  let start = `Player_turn, player_stats, boss_stats, 0, [] in
  let queue =
    Pairing_heap.of_list
      [ start ]
      ~cmp:(Comparable.lift ~f:(fun (_, _, _, mana_spent, _) -> mana_spent) Int.compare)
  in
  let visited = Hash_set.of_list (module State) [ start ] in
  with_return (fun { return } ->
    while not (Pairing_heap.is_empty queue) do
      let turn, player_stats, boss_stats, mana_spent, spells =
        Pairing_heap.pop_exn queue
      in
      if not (Stats.is_dead player_stats)
      then (
        if Stats.is_dead boss_stats then return (mana_spent, List.rev spells);
        let add_unless_visited state =
          match Hash_set.strict_add visited state with
          | Error _ -> ()
          | Ok () -> Pairing_heap.add queue state
        in
        match turn with
        | `Player_turn ->
          player_turn_neighbors ~player_stats ~boss_stats ~mana_spent ~spells
          |> List.iter ~f:add_unless_visited
        | `Boss_turn ->
          add_unless_visited
            (boss_turn_neighbor ~player_stats ~boss_stats ~mana_spent ~spells))
    done;
    assert false)
;;

let a () =
  let%bind boss_stats = Lazy_deferred.force_exn input in
  let player_stats =
    Stats.create ~hp:50 ~mp:500 ~dmg:0 ~effects:(Map.empty (module Effect))
  in
  let mana_spent, spells =
    min_mana_for_player_win ~player_stats ~boss_stats ~hard:false
  in
  print_s [%sexp (mana_spent : int), (spells : Spell.t list)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect
    {|
    (900
     (Poison Recharge Magic_missile Poison Magic_missile Shield Magic_missile
      Magic_missile)) |}];
  return ()
;;

let b () =
  let%bind boss_stats = Lazy_deferred.force_exn input in
  let player_stats =
    Stats.create ~hp:50 ~mp:500 ~dmg:0 ~effects:(Map.empty (module Effect))
  in
  let mana_spent, spells = min_mana_for_player_win ~player_stats ~boss_stats ~hard:true in
  print_s [%sexp (mana_spent : int), (spells : Spell.t list)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect
    {| (1216 (Poison Recharge Shield Poison Recharge Drain Poison Magic_missile)) |}];
  return ()
;;
