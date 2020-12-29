open! Core
open! Async
open! Import

let use_example_input = false
let g = 7

module Input = struct
  open! Advent_of_code_input_helpers

  type t =
    { pk1 : int
    ; pk2 : int
    }
  [@@deriving sexp_of]

  let parse input : t =
    match input |> lines |> List.map ~f:Int.of_string with
    | [ pk1; pk2 ] -> { pk1; pk2 }
    | _ -> failwith "parse"
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () ->
      Reader.file_contents
        (if use_example_input then "input.example.txt" else "input.txt")
      >>| parse)
  ;;
end

let modulus = 20201227

let discrete_log g x modulus =
  Sequence.repeat ()
  |> Sequence.folding_map ~init:1 ~f:(fun product () -> product * g % modulus, product)
  |> Sequence.findi ~f:(fun _ x' -> x = x')
  |> uw
  |> fst
;;

let powmod = Euler.Number_theory.Int.powmod ~modulus

let a () =
  let%bind ({ pk1; pk2 } : Input.t) = Lazy_deferred.force_exn Input.t in
  let enc_key = powmod pk1 (discrete_log g pk2 modulus) in
  print_s [%sexp (enc_key : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 18329280 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn Input.t in
  print_s [%sexp (input : Input.t)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {|
    ((pk1 12092626)
     (pk2 4707356)) |}] in
  return ()
;;
