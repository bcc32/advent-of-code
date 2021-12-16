open! Core
open! Async
open! Import

let width = 25
let height = 6

let input () =
  let%map pixels = Reader.file_contents "input" >>| String.strip in
  String.to_list pixels |> List.groupi ~break:(fun i _ _ -> i % (width * height) = 0)
;;

let a () =
  let%bind layers = input () in
  let l =
    List.min_elt
      layers
      ~compare:
        (Comparable.lift [%compare: int] ~f:(fun layer ->
           List.count layer ~f:(Char.( = ) '0')))
    |> Option.value_exn
  in
  let ones = List.count l ~f:(Char.( = ) '1') in
  let twos = List.count l ~f:(Char.( = ) '2') in
  ones * twos |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2318 |}]
;;

let empty_image () = Array.make_matrix ~dimx:height ~dimy:width 2

let b () =
  let%bind layers = input () in
  let image = empty_image () in
  List.iter layers ~f:(fun layer ->
    List.iteri layer ~f:(fun i pix ->
      let x = i % width in
      let y = i / width in
      if image.(y).(x) = 2 then image.(y).(x) <- Char.get_digit_exn pix));
  Array.map image ~f:(fun row ->
    Array.map row ~f:(function
      | 0 -> " "
      | 1 -> "#"
      | _ -> assert false)
    |> String.concat_array)
  |> String.concat_array ~sep:"\n"
  |> print_endline;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect
    {|
     ##  #  # ####  ##  ###
    #  # #  # #    #  # #  #
    #  # #### ###  #    ###
    #### #  # #    #    #  #
    #  # #  # #    #  # #  #
    #  # #  # #     ##  ### |}]
;;
