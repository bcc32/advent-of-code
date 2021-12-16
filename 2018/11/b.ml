open! Core
open! Async
open! Import

let main () =
  let%bind powers = Cell.all_powers () in
  let max_total_power = ref Int.min_value in
  let max_total_power_arg = ref (-1, -1, -1) in
  for i = 0 to Array.length powers - 1 do
    for j = 0 to Array.length powers - 1 do
      let sum = ref powers.(i).(j) in
      for size = 1 to Int.min (Array.length powers - i) (Array.length powers - j) do
        for ii = 0 to size - 1 do
          sum := !sum + powers.(i + ii).(j + size - 1)
        done;
        for jj = 0 to size - 1 do
          sum := !sum + powers.(i + size - 1).(j + jj)
        done;
        sum := !sum - powers.(i + size - 1).(j + size - 1);
        if !sum > !max_total_power
        then (
          max_total_power := !sum;
          max_total_power_arg := i + 1, j + 1, size)
      done
    done
  done;
  let x, y, size = !max_total_power_arg in
  printf "%d,%d,%d\n" x y size;
  return ()
;;

let%expect_test "b" =
  let%bind () = main () in
  [%expect {| 241,65,10 |}]
;;
