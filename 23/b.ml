open! Core
open Euler

let () =
  let is_prime = Number_theory.prime_sieve 125400 in
  Sequence.range 108400 125400 ~stop:`inclusive
  |> Sequence.filter ~f:(fun x -> (x - 108400) % 17 = 0)
  |> Sequence.count ~f:(fun x -> not is_prime.(x))
  |> printf "%d\n"
;;
