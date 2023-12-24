open! Core
open! Async
open! Import

let bit_length = 36

module Mask = struct
  module Digit = struct
    type t =
      | Floating
      | Zero
      | One
    [@@deriving sexp_of]
  end

  type t = Digit.t array [@@deriving sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      assert (Array.length t = bit_length))
  ;;

  (* It doesn't matter what this is because the program will always start with a
     [mask =] instruction. *)
  let dummy = Array.create ~len:bit_length Digit.Floating

  let re =
    let open Re in
    repn (set "X01") 36 (Some 36)
  ;;

  let of_string string =
    let bits =
      string
      |> String.to_array
      |> Array.map ~f:(fun digit : Digit.t ->
        match digit with
        | 'X' -> Floating
        | '0' -> Zero
        | '1' -> One
        | _ -> failwith "invalid digit")
    in
    (* Put in LSB-first order *)
    Array.rev_inplace bits;
    invariant bits;
    bits
  ;;
end

module Int_or_bits = struct
  type t =
    { int : int
    ; bits : bool array
    }
  [@@deriving fields, sexp_of]

  let of_string string =
    let int = Int.of_string string in
    let bits = Array.init bit_length ~f:(fun bit -> (int lsr bit) land 1 <> 0) in
    { int; bits }
  ;;

  let of_bits bits =
    let int =
      Array.fold_right bits ~init:0 ~f:(fun bit accum -> (2 * accum) + Bool.to_int bit)
    in
    { int; bits }
  ;;
end

module Instruction = struct
  type t =
    | Mask of Mask.t
    | Assign of
        { index : Int_or_bits.t
        ; value : Int_or_bits.t
        }
  [@@deriving sexp_of]

  let of_string =
    let pattern =
      let open Re in
      alt
        [ group (seq [ str "mask = "; group Mask.re ])
        ; group (seq [ str "mem["; group (rep1 digit); str "] = "; group (rep1 digit) ])
        ]
      |> compile
    in
    fun line ->
      let g = Re.exec pattern line in
      if Re.Group.test g 1
      then Re.Group.get g 2 |> Mask.of_string |> Mask
      else (
        let index = Re.Group.get g 4 |> Int_or_bits.of_string in
        let value = Re.Group.get g 5 |> Int_or_bits.of_string in
        Assign { index; value })
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Instruction.t list [@@deriving sexp_of]

  let parse input : t = input |> String.split_lines |> List.map ~f:Instruction.of_string

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "aoc.in" >>| parse)
  ;;
end

let run mem (instructions : Input.t) =
  let current_mask = ref Mask.dummy in
  List.iter instructions ~f:(function
    | Mask mask -> current_mask := mask
    | Assign { index; value } ->
      let index = index.int in
      let real_value =
        Array.map2_exn value.bits !current_mask ~f:(fun value_bit mask_bit ->
          match mask_bit with
          | Floating -> value_bit
          | Zero -> false
          | One -> true)
        |> Int_or_bits.of_bits
      in
      Hashtbl.set mem ~key:index ~data:real_value)
;;

let a () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let mem = Hashtbl.create (module Int) in
  run mem instructions;
  Hashtbl.data mem
  |> List.sum (module Int) ~f:Int_or_bits.int
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 17028179706934 |}];
  return ()
;;

let iterate_matching_indices ~(provided_index : Int_or_bits.t) ~(mask : Mask.t) ~f =
  let rec loop bit accum =
    if bit = 36
    then f accum
    else (
      match mask.(bit) with
      | Floating ->
        loop (bit + 1) ((2 * accum) + 0);
        loop (bit + 1) ((2 * accum) + 1)
      | Zero -> loop (bit + 1) ((2 * accum) + Bool.to_int provided_index.bits.(bit))
      | One -> loop (bit + 1) ((2 * accum) + 1))
  in
  loop 0 0
;;

let run mem (instructions : Input.t) =
  let current_mask = ref Mask.dummy in
  List.iter instructions ~f:(function
    | Mask mask -> current_mask := mask
    | Assign { index; value } ->
      iterate_matching_indices ~provided_index:index ~mask:!current_mask ~f:(fun i ->
        Hashtbl.set mem ~key:i ~data:value))
;;

let b () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let mem = Hashtbl.create (module Int) in
  run mem instructions;
  Hashtbl.data mem
  |> List.sum (module Int) ~f:Int_or_bits.int
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 3683236147222 |}];
  return ()
;;
