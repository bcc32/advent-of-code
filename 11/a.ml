open! Core
open! Async
open! Import

let main () =
  let%bind powers = Cell.all_powers () in
  let max_total_power = ref Int.min_value in
  let max_total_power_arg = ref (-1, -1) in
  for i = 0 to Array.length powers - 3 do
    for j = 0 to Array.length powers - 3 do
      let sum = ref 0 in
      for ii = 0 to 2 do
        for jj = 0 to 2 do
          sum := !sum + powers.(i + ii).(j + jj)
        done
      done;
      if !sum > !max_total_power
      then (
        max_total_power := !sum;
        max_total_power_arg := i + 1, j + 1)
    done
  done;
  let x, y = !max_total_power_arg in
  printf "%d,%d\n" x y;
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 235,31 |}]
;;
