open! Core

type patt = char array array
[@@deriving sexp]

type rule =
  | Two of patt * patt
  | Three of patt * patt
[@@deriving sexp]

let to_array pattern =
  String.split pattern ~on:'/'
  |> List.map ~f:String.to_list
  |> List.map ~f:List.to_array
  |> List.to_array
;;

let rotate_cw grid =
  let grid = Array.transpose_exn grid in
  Array.rev_inplace grid;
  grid
;;

let flip x =
  let x = Array.copy x in
  Array.rev_inplace x;
  x
;;

let turn x = Array.map x ~f:flip

let is_match pattern grid i j =
  let len = Array.length pattern in
  let subgrid =
    Array.sub grid ~pos:i ~len
    |> Array.map ~f:(Array.sub ~pos:j ~len)
  in
  let is = [%compare.equal: char array array] pattern in
  let g0 = subgrid in
  let g1 = g0 |> rotate_cw in
  let g2 = g1 |> rotate_cw in
  let g3 = g2 |> rotate_cw in
  is g0 || is (flip g0)
  || is g1 || is (flip g1)
  || is g2 || is (flip g2)
  || is g3 || is (flip g3)
;;

let apply_rules grid rules =
  let new_array =
    let new_size =
      if Array.length grid % 2 = 0
      then (3 * Array.length grid / 2)
      else (4 * Array.length grid / 3)
    in
    Array.make_matrix 'x' ~dimx:new_size ~dimy:new_size
  in
  let amount = if Array.length grid % 2 = 0 then 2 else 3 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if i % amount = 0 && j % amount = 0
      then (
        with_return (fun { return } ->
          List.iter rules ~f:(function
            | Two (left, right)
            | Three (left, right) ->
              if is_match left grid i j
              then (
                let i = if Array.length grid % 2 = 0 then 3 * i / 2 else 4 * i / 3 in
                let j = if Array.length grid % 2 = 0 then 3 * j / 2 else 4 * j / 3 in
                for ii = 0 to amount do
                  for jj = 0 to amount do
                    new_array.(i + ii).(j + jj) <- right.(ii).(jj)
                  done
                done;
                return ()))))
    done
  done;
  new_array
;;

let () =
  let rules =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
      match String.split line ~on:'=' with
      | [ left; right ] ->
        let left = String.strip left |> to_array in
        let right = String.strip (String.subo right ~pos:1) |> to_array in
        if Array.length left = 2
        then (Two (left, right))
        else (Three (left, right))
      | _ -> assert false)
  in
  let grid = ref (".#./..#/###" |> to_array) in
  for _ = 1 to 18 do
    if Array.length !grid % 2 = 0
    then (grid := apply_rules !grid (List.filter rules ~f:(function | Two _ -> true | Three _ -> false)))
    else (grid := apply_rules !grid (List.filter rules ~f:(function | Two _ -> false | Three _ -> true)))
  done;
  Array.sum (module Int) !grid ~f:(fun row -> Array.count row ~f:(fun x -> x = '#'))
  |> printf "%d\n"
;;
