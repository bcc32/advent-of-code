open! Core
open! Async
open! Import

let try_ elf_attack_power lines =
  let battlefield = Battlefield.create lines ~elf_attack_power in
  let rec loop rounds =
    match Battlefield.perform_round battlefield with
    | () -> loop (rounds + 1)
    | exception Battlefield.End_of_combat { winning_team = Elf }
      when Battlefield.kill_count battlefield Elf = 0 -> Some (battlefield, rounds)
    | exception Battlefield.End_of_combat _ -> None
  in
  loop 0
;;

let main input =
  let lines = input |> String.strip |> String.split_lines in
  let rec loop elf_attack_power =
    match try_ elf_attack_power lines with
    | None -> loop (elf_attack_power + 1)
    | Some (battlefield, full_rounds) ->
      let outcome = full_rounds * Battlefield.sum_of_hit_points battlefield in
      printf
        !"%d\n%{Battlefield#hum}\n%d rounds, %d hit points"
        outcome
        battlefield
        full_rounds
        (Battlefield.sum_of_hit_points battlefield)
  in
  loop 4;
  return ()
;;

let%expect_test "b" =
  let%bind lines = Reader.file_contents "input" in
  let%bind () = main lines in
  [%expect
    {|
    57820
    ################################
    ###################..###########
    ####################.....#######
    ####################...#.#######
    ####################..##...#####
    #################..........#####
    ########..########........######
    ########..#.##..###.......######
    #######.....#..........E.....###
    #######...#...............E....#
    #########.##.......E.....E....##
    ###########.............E...#.##
    #####..####...#####........E.E.#
    ####....###..#######.....E#....#
    ####....###.#########......##..#
    ####........#########........E.#
    ###.........#########......E..##
    ##..#.......#########.........##
    #...#.......#########.........##
    #..#.....#...#######........####
    #.............#####.....#...####
    #.#.........................####
    #........#..................####
    ###...##....................####
    ###...##....................####
    ####..#####..#............######
    ##########...#.##....##...######
    ##########.....##...############
    ###########.#......#############
    #############...################
    ###############.################
    ################################
    59 rounds, 980 hit points |}];
  return ()
;;
