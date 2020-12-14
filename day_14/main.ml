open! Core
open! Async
open! Import

module Instruction = struct
  type t =
    | Mask of
        { mask_mask : int
        ; mask : int
        }
    | Assign of
        { index : int
        ; value : int
        }
  [@@deriving sexp_of]

  let of_string =
    let pattern =
      let open Re in
      alt
        [ group (seq [ str "mask = "; group (repn (set "X01") 36 (Some 36)) ])
        ; group (seq [ str "mem["; group (rep1 digit); str "] = "; group (rep1 digit) ])
        ]
      |> compile
    in
    fun line ->
      let g = Re.exec pattern line in
      if Re.Group.test g 1
      then (
        let mask_mask, mask =
          Re.Group.get g 2
          |> String.to_list
          |> List.fold ~init:(0, 0) ~f:(fun (mask_mask, mask) -> function
            | 'X' -> (2 * mask_mask) + 0, 2 * mask
            | '1' -> (2 * mask_mask) + 1, (2 * mask) + 1
            | '0' -> (2 * mask_mask) + 1, 2 * mask
            | _ -> failwith "invalid digit")
        in
        Mask { mask_mask; mask })
      else (
        let index = Re.Group.get g 4 |> Int.of_string in
        let value = Re.Group.get g 5 |> Int.of_string in
        Assign { index; value })
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Instruction.t list [@@deriving sexp_of]

  let parse input : t = input |> String.split_lines |> List.map ~f:Instruction.of_string

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let run mem (instructions : Input.t) =
  let current_mask = ref (0, 0) in
  List.iter instructions ~f:(function
    | Mask { mask_mask; mask } -> current_mask := mask_mask, mask
    | Assign { index; value } ->
      let mask_mask, mask = !current_mask in
      let real_value = lnot mask_mask land value lor (mask_mask land mask) in
      Hashtbl.set mem ~key:index ~data:real_value)
;;

let a () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let mem = Hashtbl.create (module Int) in
  run mem instructions;
  Hashtbl.data mem |> List.sum (module Int) ~f:Fn.id |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 17028179706934 |}] in
  return ()
;;

let iterate_matching_indices ~provided_index ~mask_mask ~mask ~f =
  let get_bit int bit = (int lsr Int.(36 - 1 - bit)) land 1 <> 0 in
  let rec loop bit accum =
    if bit = 36
    then f accum
    else if (* X *)
      not (get_bit mask_mask bit)
    then (
      loop (bit + 1) ((2 * accum) + 0);
      loop (bit + 1) ((2 * accum) + 1))
    else if (* 1 *)
      get_bit mask_mask bit && get_bit mask bit
    then loop (bit + 1) ((2 * accum) + 1)
    else (* 0 *)
      loop (bit + 1) ((2 * accum) + if get_bit provided_index bit then 1 else 0)
  in
  loop 0 0
;;

let run mem (instructions : Input.t) =
  let current_mask = ref (0, 0) in
  List.iter instructions ~f:(function
    | Mask { mask_mask; mask } -> current_mask := mask_mask, mask
    | Assign { index; value } ->
      let mask_mask, mask = !current_mask in
      iterate_matching_indices
        ~provided_index:(Int.of_int index)
        ~mask_mask
        ~mask
        ~f:(fun i -> Hashtbl.set mem ~key:i ~data:value))
;;

let b () =
  let%bind instructions = Lazy_deferred.force_exn Input.t in
  let mem = Hashtbl.create (module Int) in
  run mem instructions;
  Hashtbl.data mem |> List.sum (module Int) ~f:Fn.id |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 3683236147222 |}] in
  return ()
;;
