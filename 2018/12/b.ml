open! Core
open! Async
open! Import

let demo () =
  let%bind game = Game.read () in
  let (_ : State.t) =
    let rec loop state n =
      print_s
        [%message
          "loop"
            ~sum:(State.sum_plants state : int)
            ~plants:(State.count_plants state : int)];
      if n > 0 then loop (State.step state game.step) (n - 1) else state
    in
    loop (State.of_init game.init) 200
  in
  return ()
;;

(* Converges on size 81 with increment of 81 per step. *)
let%expect_test "b demo" =
  let%bind () = demo () in
  [%expect
    {|
    (loop
      (sum    2936)
      (plants 56))
    (loop
      (sum    3078)
      (plants 58))
    (loop
      (sum    2667)
      (plants 54))
    (loop
      (sum    2663)
      (plants 55))
    (loop
      (sum    2759)
      (plants 58))
    (loop
      (sum    2747)
      (plants 58))
    (loop
      (sum    2844)
      (plants 59))
    (loop
      (sum    2744)
      (plants 58))
    (loop
      (sum    2950)
      (plants 61))
    (loop
      (sum    3087)
      (plants 62))
    (loop
      (sum    3137)
      (plants 64))
    (loop
      (sum    2566)
      (plants 55))
    (loop
      (sum    2777)
      (plants 57))
    (loop
      (sum    2924)
      (plants 60))
    (loop
      (sum    2675)
      (plants 55))
    (loop
      (sum    2738)
      (plants 56))
    (loop
      (sum    2885)
      (plants 57))
    (loop
      (sum    3094)
      (plants 58))
    (loop
      (sum    3317)
      (plants 64))
    (loop
      (sum    3265)
      (plants 63))
    (loop
      (sum    3061)
      (plants 56))
    (loop
      (sum    3287)
      (plants 60))
    (loop
      (sum    3402)
      (plants 62))
    (loop
      (sum    3474)
      (plants 63))
    (loop
      (sum    3572)
      (plants 61))
    (loop
      (sum    3656)
      (plants 63))
    (loop
      (sum    3561)
      (plants 62))
    (loop
      (sum    3799)
      (plants 66))
    (loop
      (sum    3622)
      (plants 63))
    (loop
      (sum    3973)
      (plants 68))
    (loop
      (sum    3965)
      (plants 67))
    (loop
      (sum    4134)
      (plants 70))
    (loop
      (sum    4015)
      (plants 66))
    (loop
      (sum    4272)
      (plants 70))
    (loop
      (sum    4301)
      (plants 70))
    (loop
      (sum    4402)
      (plants 70))
    (loop
      (sum    4643)
      (plants 73))
    (loop
      (sum    4630)
      (plants 72))
    (loop
      (sum    4656)
      (plants 71))
    (loop
      (sum    4719)
      (plants 70))
    (loop
      (sum    4453)
      (plants 64))
    (loop
      (sum    4858)
      (plants 68))
    (loop
      (sum    4877)
      (plants 68))
    (loop
      (sum    4816)
      (plants 70))
    (loop
      (sum    4917)
      (plants 71))
    (loop
      (sum    5112)
      (plants 73))
    (loop
      (sum    5133)
      (plants 74))
    (loop
      (sum    4878)
      (plants 70))
    (loop
      (sum    5408)
      (plants 71))
    (loop
      (sum    5536)
      (plants 73))
    (loop
      (sum    4184)
      (plants 59))
    (loop
      (sum    4311)
      (plants 61))
    (loop
      (sum    4593)
      (plants 66))
    (loop
      (sum    4810)
      (plants 66))
    (loop
      (sum    4889)
      (plants 67))
    (loop
      (sum    5079)
      (plants 69))
    (loop
      (sum    5080)
      (plants 70))
    (loop
      (sum    5327)
      (plants 74))
    (loop
      (sum    5387)
      (plants 76))
    (loop
      (sum    5450)
      (plants 73))
    (loop
      (sum    5468)
      (plants 74))
    (loop
      (sum    5725)
      (plants 76))
    (loop
      (sum    5724)
      (plants 76))
    (loop
      (sum    5719)
      (plants 76))
    (loop
      (sum    5857)
      (plants 78))
    (loop
      (sum    5750)
      (plants 75))
    (loop
      (sum    6052)
      (plants 78))
    (loop
      (sum    6307)
      (plants 79))
    (loop
      (sum    6809)
      (plants 84))
    (loop
      (sum    5336)
      (plants 68))
    (loop
      (sum    5529)
      (plants 70))
    (loop
      (sum    5539)
      (plants 70))
    (loop
      (sum    5627)
      (plants 71))
    (loop
      (sum    5704)
      (plants 71))
    (loop
      (sum    5691)
      (plants 71))
    (loop
      (sum    5580)
      (plants 69))
    (loop
      (sum    6023)
      (plants 74))
    (loop
      (sum    6008)
      (plants 73))
    (loop
      (sum    5450)
      (plants 66))
    (loop
      (sum    5444)
      (plants 65))
    (loop
      (sum    5666)
      (plants 67))
    (loop
      (sum    5828)
      (plants 68))
    (loop
      (sum    5799)
      (plants 67))
    (loop
      (sum    5709)
      (plants 65))
    (loop
      (sum    5906)
      (plants 67))
    (loop
      (sum    5981)
      (plants 67))
    (loop
      (sum    5755)
      (plants 63))
    (loop
      (sum    5834)
      (plants 63))
    (loop
      (sum    5889)
      (plants 63))
    (loop
      (sum    6212)
      (plants 67))
    (loop
      (sum    6351)
      (plants 68))
    (loop
      (sum    6546)
      (plants 70))
    (loop
      (sum    6291)
      (plants 65))
    (loop
      (sum    6286)
      (plants 64))
    (loop
      (sum    6157)
      (plants 61))
    (loop
      (sum    6333)
      (plants 63))
    (loop
      (sum    6524)
      (plants 65))
    (loop
      (sum    6644)
      (plants 66))
    (loop
      (sum    6444)
      (plants 62))
    (loop
      (sum    6547)
      (plants 63))
    (loop
      (sum    6707)
      (plants 65))
    (loop
      (sum    6993)
      (plants 69))
    (loop
      (sum    6870)
      (plants 66))
    (loop
      (sum    6927)
      (plants 66))
    (loop
      (sum    6867)
      (plants 64))
    (loop
      (sum    6982)
      (plants 65))
    (loop
      (sum    7092)
      (plants 66))
    (loop
      (sum    7033)
      (plants 64))
    (loop
      (sum    7141)
      (plants 65))
    (loop
      (sum    7395)
      (plants 69))
    (loop
      (sum    7427)
      (plants 69))
    (loop
      (sum    7307)
      (plants 65))
    (loop
      (sum    7452)
      (plants 67))
    (loop
      (sum    7413)
      (plants 65))
    (loop
      (sum    7535)
      (plants 66))
    (loop
      (sum    7724)
      (plants 69))
    (loop
      (sum    7678)
      (plants 67))
    (loop
      (sum    7806)
      (plants 68))
    (loop
      (sum    7865)
      (plants 68))
    (loop
      (sum    7900)
      (plants 68))
    (loop
      (sum    8117)
      (plants 71))
    (loop
      (sum    8226)
      (plants 72))
    (loop
      (sum    8141)
      (plants 69))
    (loop
      (sum    8319)
      (plants 71))
    (loop
      (sum    8423)
      (plants 72))
    (loop
      (sum    8493)
      (plants 72))
    (loop
      (sum    8568)
      (plants 72))
    (loop
      (sum    8637)
      (plants 72))
    (loop
      (sum    8707)
      (plants 72))
    (loop
      (sum    8929)
      (plants 74))
    (loop
      (sum    8998)
      (plants 74))
    (loop
      (sum    8982)
      (plants 73))
    (loop
      (sum    9047)
      (plants 73))
    (loop
      (sum    9128)
      (plants 73))
    (loop
      (sum    9111)
      (plants 72))
    (loop
      (sum    9103)
      (plants 71))
    (loop
      (sum    9400)
      (plants 74))
    (loop
      (sum    9391)
      (plants 73))
    (loop
      (sum    9524)
      (plants 74))
    (loop
      (sum    9446)
      (plants 72))
    (loop
      (sum    9587)
      (plants 73))
    (loop
      (sum    9587)
      (plants 72))
    (loop
      (sum    9782)
      (plants 74))
    (loop
      (sum    9791)
      (plants 73))
    (loop
      (sum    9979)
      (plants 75))
    (loop
      (sum    9988)
      (plants 74))
    (loop
      (sum    10184)
      (plants 76))
    (loop
      (sum    10315)
      (plants 77))
    (loop
      (sum    10315)
      (plants 76))
    (loop
      (sum    10511)
      (plants 78))
    (loop
      (sum    10520)
      (plants 77))
    (loop
      (sum    10658)
      (plants 78))
    (loop
      (sum    10864)
      (plants 80))
    (loop
      (sum    10885)
      (plants 79))
    (loop
      (sum    10818)
      (plants 77))
    (loop
      (sum    10951)
      (plants 78))
    (loop
      (sum    11094)
      (plants 79))
    (loop
      (sum    11237)
      (plants 80))
    (loop
      (sum    11373)
      (plants 81))
    (loop
      (sum    11454)
      (plants 81))
    (loop
      (sum    11535)
      (plants 81))
    (loop
      (sum    11616)
      (plants 81))
    (loop
      (sum    11697)
      (plants 81))
    (loop
      (sum    11778)
      (plants 81))
    (loop
      (sum    11859)
      (plants 81))
    (loop
      (sum    11940)
      (plants 81))
    (loop
      (sum    12021)
      (plants 81))
    (loop
      (sum    12102)
      (plants 81))
    (loop
      (sum    12183)
      (plants 81))
    (loop
      (sum    12264)
      (plants 81))
    (loop
      (sum    12345)
      (plants 81))
    (loop
      (sum    12426)
      (plants 81))
    (loop
      (sum    12507)
      (plants 81))
    (loop
      (sum    12588)
      (plants 81))
    (loop
      (sum    12669)
      (plants 81))
    (loop
      (sum    12750)
      (plants 81))
    (loop
      (sum    12831)
      (plants 81))
    (loop
      (sum    12912)
      (plants 81))
    (loop
      (sum    12993)
      (plants 81))
    (loop
      (sum    13074)
      (plants 81))
    (loop
      (sum    13155)
      (plants 81))
    (loop
      (sum    13236)
      (plants 81))
    (loop
      (sum    13317)
      (plants 81))
    (loop
      (sum    13398)
      (plants 81))
    (loop
      (sum    13479)
      (plants 81))
    (loop
      (sum    13560)
      (plants 81))
    (loop
      (sum    13641)
      (plants 81))
    (loop
      (sum    13722)
      (plants 81))
    (loop
      (sum    13803)
      (plants 81))
    (loop
      (sum    13884)
      (plants 81))
    (loop
      (sum    13965)
      (plants 81))
    (loop
      (sum    14046)
      (plants 81))
    (loop
      (sum    14127)
      (plants 81))
    (loop
      (sum    14208)
      (plants 81))
    (loop
      (sum    14289)
      (plants 81))
    (loop
      (sum    14370)
      (plants 81))
    (loop
      (sum    14451)
      (plants 81))
    (loop
      (sum    14532)
      (plants 81))
    (loop
      (sum    14613)
      (plants 81))
    (loop
      (sum    14694)
      (plants 81))
    (loop
      (sum    14775)
      (plants 81)) |}]
;;

let main () =
  printf "%d\n" (14_775 + (81 * (50_000_000_000 - 200)));
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 4049999998575 |}]
;;
