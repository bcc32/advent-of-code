open! Core
open! Async
open! Import

let parse lines =
  let grid = lines |> Array.of_list |> Array.map ~f:String.to_array in
  grid |> Array.map ~f:(Array.map ~f:(Char.( = ) '#'))
;;

let part = ref 1

let get grid i j =
  if !part <> 1
  then
    ((i = 0 || i = Array.length grid - 1) && (j = 0 || j = Array.length grid.(0) - 1))
    || grid.(i).(j)
  else grid.(i).(j)
;;

let count grid =
  let c = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if get grid i j then incr c
    done
  done;
  !c
;;

let tick grid =
  let get = get grid in
  let grid' = Array.map grid ~f:Array.copy in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      let count = ref 0 in
      for di = -1 to 1 do
        for dj = -1 to 1 do
          if di <> 0 || dj <> 0
          then
            if Int.between (i + di) ~low:0 ~high:(Array.length grid - 1)
            && Int.between (j + dj) ~low:0 ~high:(Array.length grid.(0) - 1)
            then if get (i + di) (j + dj) then incr count
        done
      done;
      grid'.(i).(j) <- (if get i j then !count = 2 || !count = 3 else !count = 3)
    done
  done;
  grid'
;;

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "input.txt" >>| String.split_lines >>| parse)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  let input = ref input in
  for _ = 1 to 100 do
    input := tick !input
  done;
  let count = count !input in
  print_s [%sexp (count : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 821 |}] in
  return ()
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  part := 2;
  let input = ref input in
  for _ = 1 to 100 do
    input := tick !input
  done;
  let count = count !input in
  print_s [%sexp (count : int)];
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 886 |}] in
  return ()
;;
