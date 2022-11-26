open! Core
open! Async
open! Import

type ing =
  { name : string
  ; capacity : int
  ; durability : int
  ; flavor : int
  ; texture : int
  ; calories : int
  }

let parse_ingredient_line =
  let parser =
    let open Angstrom in
    let num =
      let* sign = option "" (string "-") in
      let+ digits = take_while1 Char.is_digit in
      Int.of_string (sign ^ digits)
    in
    let* name = take_while1 Char.is_alpha in
    let* _ = string ": capacity " in
    let* capacity = num in
    let* _ = string ", durability " in
    let* durability = num in
    let* _ = string ", flavor " in
    let* flavor = num in
    let* _ = string ", texture " in
    let* texture = num in
    let* _ = string ", calories " in
    let* calories = num in
    return { name; capacity; durability; flavor; texture; calories }
  in
  fun line ->
    Angstrom.parse_string parser line ~consume:All
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt"
    >>| (String.split_lines >> List.map ~f:parse_ingredient_line))
;;

let score ?(cal = Fn.const true) ingredients =
  let { name = _; capacity; durability; flavor; texture; calories } =
    List.map ingredients ~f:(fun (amt, ing) ->
      { name = ing.name
      ; capacity = amt * ing.capacity
      ; durability = amt * ing.durability
      ; flavor = amt * ing.flavor
      ; texture = amt * ing.texture
      ; calories = amt * ing.calories
      })
    |> List.reduce_exn ~f:(fun i1 i2 ->
      { name = i1.name
      ; capacity = i1.capacity + i2.capacity
      ; durability = i1.durability + i2.durability
      ; flavor = i1.flavor + i2.flavor
      ; texture = i1.texture + i2.texture
      ; calories = i1.calories + i2.calories
      })
  in
  if cal calories
  then (
    let capacity = Int.max capacity 0 in
    let durability = Int.max durability 0 in
    let flavor = Int.max flavor 0 in
    let texture = Int.max texture 0 in
    capacity * durability * flavor * texture)
  else Int.min_value
;;

let find_highest ?cal ings =
  let max_s = ref Int.min_value in
  match ings with
  | [ i1; i2; i3; i4 ] ->
    for amt1 = 0 to 100 do
      for amt2 = 0 to 100 - amt1 do
        for amt3 = 0 to 100 - amt1 - amt2 do
          let amt4 = 100 - amt1 - amt2 - amt3 in
          let s = score ?cal [ amt1, i1; amt2, i2; amt3, i3; amt4, i4 ] in
          max_s := Int.max !max_s s
        done
      done
    done;
    !max_s
  | _ -> assert false
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = find_highest input in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 21367368 |}];
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  let ans = find_highest input ~cal:(( = ) 500) in
  print_s [%sexp (ans : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 1766400 |}];
  return ()
;;
