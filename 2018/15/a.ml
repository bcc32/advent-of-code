open! Core
open! Async
open! Import

let main input =
  let lines = input |> String.strip |> String.split_lines in
  let battlefield = Battlefield.create lines in
  let full_rounds =
    let rec loop rounds =
      match Battlefield.perform_round battlefield with
      | () -> loop (rounds + 1)
      | exception Battlefield.End_of_combat { winning_team = _ } -> rounds
    in
    loop 0
  in
  let outcome = full_rounds * Battlefield.sum_of_hit_points battlefield in
  printf
    !"%d\n%{Battlefield#hum}\n%d rounds, %d hit points"
    outcome
    battlefield
    full_rounds
    (Battlefield.sum_of_hit_points battlefield);
  return ()
;;

let%expect_test "a" =
  let%bind lines = Reader.file_contents "input" in
  let%bind () = main lines in
  [%expect
    {|
    189910
    ################################
    ###################..###########
    ####################.....#######
    ####################...#.#######
    ####################..##...#####
    #################..........#####
    ########..########........######
    ########..#.##..###.......######
    #######.....#................###
    #######...#.............G......#
    #########.##..................##
    ###########.............G...#.##
    #####..####...#####...G.....G..#
    ####....###..#######.G.G..#....#
    ####....###.#########.G.G..##..#
    ####.......G#########..G..G....#
    ###.........#########G........##
    ##..#.......#########.........##
    #...#.......#########.........##
    #..#.....#...#######........####
    #.............#####.....#...####
    #.#...........G.............####
    #........#GGG...............####
    ###...##...G................####
    ###...##....................####
    ####..#####..#............######
    ##########...#.##....##...######
    ##########.....##...############
    ###########.#......#############
    #############...################
    ###############.################
    ################################
    70 rounds, 2713 hit points |}];
  return ()
;;

let%test_module "examples" =
  (module struct
    let%expect_test _ =
      let input = {|
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
|} in
      let%bind () = main input in
      [%expect
        {|
        27730
        #######
        #G....#
        #.G...#
        #.#.#G#
        #...#.#
        #....G#
        #######
        47 rounds, 590 hit points |}];
      return ()
    ;;

    let%expect_test _ =
      let input = {|
#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######
|} in
      let%bind () = main input in
      [%expect
        {|
        36334
        #######
        #...#E#
        #E#...#
        #.E##.#
        #E..#E#
        #.....#
        #######
        37 rounds, 982 hit points |}];
      return ()
    ;;

    let%expect_test _ =
      let input = {|
#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######
|} in
      let%bind () = main input in
      [%expect
        {|
        39514
        #######
        #.E.E.#
        #.#E..#
        #E.##.#
        #.E.#.#
        #...#.#
        #######
        46 rounds, 859 hit points |}];
      return ()
    ;;

    let%expect_test _ =
      let input = {|
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######
|} in
      let%bind () = main input in
      [%expect
        {|
        27755
        #######
        #G.G#.#
        #.#G..#
        #..#..#
        #...#G#
        #...G.#
        #######
        35 rounds, 793 hit points |}];
      return ()
    ;;

    let%expect_test _ =
      let input = {|
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
|} in
      let%bind () = main input in
      [%expect
        {|
        28944
        #######
        #.....#
        #.#G..#
        #.###.#
        #.#.#.#
        #G.G#G#
        #######
        54 rounds, 536 hit points |}];
      return ()
    ;;

    let%expect_test _ =
      let input =
        {|
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
|}
      in
      let%bind () = main input in
      [%expect
        {|
        18740
        #########
        #.G.....#
        #G.G#...#
        #.G##...#
        #...##..#
        #.G.#...#
        #.......#
        #.......#
        #########
        20 rounds, 937 hit points |}];
      return ()
    ;;
  end)
;;
