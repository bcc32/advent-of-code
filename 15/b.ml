open! Core

let modulo = 2147483647

let a_factor = 16807
let b_factor = 48271

let () =
  let input =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      String.subo line ~pos:24
      |> Int.of_string)
  in
  let count = ref 0 in
  match input with
  | [ a; b ] ->
    let a = ref a in
    let b = ref b in
    for _ = 1 to 5_000_000 do
      a := !a * a_factor % modulo;
      while !a % 4 <> 0 do
        a := !a * a_factor % modulo
      done;
      b := !b * b_factor % modulo;
      while !b % 8 <> 0 do
        b := !b * b_factor % modulo
      done;
      if (!a % (1 lsl 16)) = (!b % (1 lsl 16))
      then (incr count)
    done;
    printf "%d\n" !count
  | _ -> invalid_arg "input count"
;;
