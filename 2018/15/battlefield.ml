open! Core
open! Async
open! Import

module Square = struct
  type t =
    | Open
    | Wall
    | Unit of Game_unit.t
  [@@deriving sexp_of]

  let is_open = function
    | Open -> true
    | Wall | Unit _ -> false
  ;;
end

exception End_of_combat of { winning_team : Team.t }

type t =
  { grid : Square.t array array
  ; mutable kill_count : int Team.Total_map.t
  }
[@@deriving sexp_of]

let to_string_hum t =
  Array.to_list t.grid
  |> List.map ~f:(fun row ->
    Array.to_list row
    |> List.map ~f:(function
      | Open -> '.'
      | Wall -> '#'
      | Unit u -> u.team |> Team.to_char)
    |> String.of_char_list)
  |> String.concat ~sep:"\n"
;;

let create ?(elf_attack_power = 3) lines =
  let create_unit ~loc ~team = Game_unit.create ~loc ~hit_points:200 ~team in
  let grid =
    Array.of_list_mapi lines ~f:(fun x row ->
      String.to_array row
      |> Array.mapi ~f:(fun y -> function
        | '.' -> Square.Open
        | '#' -> Wall
        | 'E' -> Unit (create_unit ~loc:{ x; y } ~team:Elf ~attack_power:elf_attack_power)
        | 'G' -> Unit (create_unit ~loc:{ x; y } ~team:Goblin ~attack_power:3)
        | char -> raise_s [%message "Unrecognized grid char" ~_:(char : char)]))
  in
  { grid; kill_count = Team.Total_map.create_const 0 }
;;

let get t { Loc.x; y } = t.grid.(x).(y)

let units t =
  Array.to_list t.grid
  |> List.concat_map ~f:(fun row ->
    Array.to_list row
    |> List.filter_map ~f:(function
      | Open -> None
      | Wall -> None
      | Unit u -> Some u))
;;

let calculate_distances t ~start =
  let q = Queue.create () in
  Queue.enqueue q start;
  let distance = Loc.Table.create () in
  Hashtbl.add_exn distance ~key:start ~data:0;
  while not (Queue.is_empty q) do
    let loc = Queue.dequeue_exn q in
    Loc.adjacent loc ~height:(Array.length t.grid) ~width:(Array.length t.grid.(0))
    |> List.iter ~f:(fun loc' ->
      if Square.is_open (get t loc')
      then (
        match Hashtbl.mem distance loc' with
        | true -> ()
        | false ->
          Hashtbl.add_exn distance ~key:loc' ~data:(Hashtbl.find_exn distance loc + 1);
          Queue.enqueue q loc'))
  done;
  distance
;;

let try_perform_attack t ~(unit : Game_unit.t) ~(targets : Game_unit.t list) =
  match
    targets
    |> List.filter ~f:(fun target -> Loc.is_adjacent target.loc unit.loc)
    |> List.min_elt
         ~compare:
           (Comparable.lexicographic
              [ Comparable.lift [%compare: int] ~f:Game_unit.hit_points
              ; Comparable.lift [%compare: Loc.t] ~f:Game_unit.loc
              ])
  with
  | None -> (* no targets, do nothing *) ()
  | Some target ->
    (match Game_unit.receive_damage target ~points:unit.attack_power with
     | `Alive -> ()
     | `Dead ->
       t.kill_count <- Total_map.change t.kill_count target.team ~f:succ;
       t.grid.(target.loc.x).(target.loc.y) <- Open)
;;

let perform_round_for_unit t ~(unit : Game_unit.t) ~units =
  let height = Array.length t.grid in
  let width = Array.length t.grid.(0) in
  match
    List.filter units ~f:(fun u ->
      Game_unit.is_alive u && not ([%equal: Team.t] u.team unit.team))
  with
  | [] -> raise (End_of_combat { winning_team = unit.team })
  | _ :: _ as targets ->
    let open_squares =
      targets
      |> List.concat_map ~f:(fun target -> Loc.adjacent target.loc ~height ~width)
      |> List.filter ~f:(fun loc ->
        match get t loc with
        | Open -> true
        | Wall -> false
        | Unit u -> phys_equal u unit)
    in
    (match List.mem open_squares unit.loc ~equal:[%equal: Loc.t] with
     | true -> try_perform_attack t ~unit ~targets
     (* already adjacent *)
     | false ->
       (* move towards an open square *)
       (match open_squares with
        | [] -> (* can't move, do nothing *) ()
        | _ :: _ ->
          let distance_to =
            let tbl = calculate_distances t ~start:unit.loc in
            fun loc -> Hashtbl.find tbl loc
          in
          (match
             List.filter_map open_squares ~f:(fun sq ->
               match distance_to sq with
               | None -> None
               | Some dist -> Some (sq, dist))
             |> List.min_elt
                  ~compare:
                    (Comparable.lexicographic
                       [ Comparable.lift [%compare: int] ~f:snd
                       ; Comparable.lift [%compare: Loc.t] ~f:fst
                       ])
           with
           | None -> (* no reachable target square, do nothing *) ()
           | Some (target_square, _) ->
             let distance_to_target =
               let tbl = calculate_distances t ~start:target_square in
               fun loc ->
                 Option.value_map (Hashtbl.find tbl loc) ~f:float ~default:Float.infinity
             in
             let step_square =
               Loc.adjacent unit.loc ~height ~width
               |> List.filter ~f:(fun loc ->
                 match get t loc with
                 | Open -> true
                 | Wall | Unit _ -> false)
               |> List.min_elt
                    ~compare:
                      (Comparable.lexicographic
                         [ Comparable.lift [%compare: float] ~f:distance_to_target
                         ; [%compare: Loc.t]
                         ])
             in
             (match step_square with
              | None -> ()
              | Some step_square ->
                let old_square = unit.loc in
                Game_unit.set_loc unit step_square;
                t.grid.(old_square.x).(old_square.y) <- Open;
                t.grid.(step_square.x).(step_square.y) <- Unit unit;
                try_perform_attack t ~unit ~targets))))
;;

let perform_round t =
  let units =
    units t |> List.sort ~compare:(Comparable.lift [%compare: Loc.t] ~f:Game_unit.loc)
  in
  List.iter units ~f:(fun unit ->
    if Game_unit.is_alive unit then perform_round_for_unit t ~unit ~units)
;;

let kill_count t team = Total_map.find t.kill_count team

let sum_of_hit_points t =
  units t
  |> List.filter ~f:Game_unit.is_alive
  |> List.sum (module Int) ~f:Game_unit.hit_points
;;
