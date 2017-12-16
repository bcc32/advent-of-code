open! Core

type move =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

let transform moves array =
  List.iter moves ~f:(function
    | Spin len ->
      let array' = Array.create ' ' ~len:16 in
      Array.blit ~src:array ~dst:array'
        ~src_pos:(Array.length array - len) ~dst_pos:0 ~len;
      Array.blit ~src:array ~dst:array'
        ~src_pos:0 ~dst_pos:len ~len:(Array.length array - len);
      Array.blito () ~src:array' ~dst:array
    | Exchange (i, j) ->
      let t = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- t
    | Partner (a, b) ->
      let (i, _) = Array.findi_exn array ~f:(fun _ p -> p = a) in
      let (j, _) = Array.findi_exn array ~f:(fun _ p -> p = b) in
      let t = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- t)
;;

let () =
  let input =
    In_channel.with_file Sys.argv.(1) ~f:In_channel.input_all
    |> String.split ~on:','
    |> List.map ~f:(fun instr ->
      let data = String.subo instr ~pos:1 in
      match instr.[0] with
      | 's' -> Spin (Int.of_string data)
      | 'x' ->
        (match String.split data ~on:'/' with
         | [ a; b ] -> Exchange (Int.of_string a, Int.of_string b)
         | _ -> failwith "expected two integers")
      | 'p' ->
        (match String.split data ~on:'/' with
         | [ a; b ] -> Partner (a.[0], b.[0])
         | _ -> failwith "expected two strings")
      | _ -> raise_s [%message "invalid command" ~command:(instr.[0] : char)])
  in
  let dry_run = Array.init 16 ~f:(fun i -> Char.of_int_exn (Char.to_int 'a' + i)) in
  let starting = Array.copy dry_run in
  let cycle_length =
    let rec loop i =
      transform input dry_run;
      if [%compare.equal: char array] dry_run starting
      then i
      else (loop (i + 1))
    in
    loop 1
  in
  let programs = Array.init 16 ~f:(fun i -> Char.of_int_exn (Char.to_int 'a' + i)) in
  for _ = 1 to 1_000_000_000 % cycle_length do
    transform input programs
  done;
  programs
  |> Array.to_list
  |> List.map ~f:String.of_char
  |> String.concat
  |> print_endline
;;
