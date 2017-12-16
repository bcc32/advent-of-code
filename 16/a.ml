open! Core

type move =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

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
  let programs = Array.init 16 ~f:(fun i -> Char.of_int_exn (Char.to_int 'a' + i)) in
  List.iter input ~f:(function
    | Spin len ->
      let programs' = Array.create ' ' ~len:16 in
      Array.blit ~src:programs ~dst:programs'
        ~src_pos:(Array.length programs - len) ~dst_pos:0 ~len;
      Array.blit ~src:programs ~dst:programs'
        ~src_pos:0 ~dst_pos:len ~len:(Array.length programs - len);
      Array.blito () ~src:programs' ~dst:programs
    | Exchange (i, j) ->
      let t = programs.(i) in
      programs.(i) <- programs.(j);
      programs.(j) <- t
    | Partner (a, b) ->
      let (i, _) = Array.findi_exn programs ~f:(fun _ p -> p = a) in
      let (j, _) = Array.findi_exn programs ~f:(fun _ p -> p = b) in
      let t = programs.(i) in
      programs.(i) <- programs.(j);
      programs.(j) <- t);
  programs
  |> Array.to_list
  |> List.map ~f:String.of_char
  |> String.concat
  |> print_endline
;;
