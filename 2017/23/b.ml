open! Core

let[@inline always] int_next_probable_prime n =
  match n mod 6 with
  | 1 -> n + 4
  | 5 -> n + 2
  | 0 -> n + 1
  | 2 -> n + 1
  | 3 -> n + 2
  | 4 -> n + 1
  | _ -> assert false
;;

let prime_sieve limit =
  let len = limit + 1 in
  let primes = Array.create ~len true in
  let rec mark p n =
    if n < len
    then (
      primes.(n) <- false;
      mark p (n + p))
  in
  let rec sieve p =
    if p * p < len
    then (
      if primes.(p) then mark p (p * p);
      sieve (int_next_probable_prime p))
  in
  primes.(0) <- false;
  primes.(1) <- false;
  sieve 2;
  primes
;;

let () =
  let is_prime = prime_sieve 125400 in
  Sequence.range 108400 125400 ~stop:`inclusive
  |> Sequence.filter ~f:(fun x -> (x - 108400) % 17 = 0)
  |> Sequence.count ~f:(fun x -> not is_prime.(x))
  |> printf "%d\n"
;;
