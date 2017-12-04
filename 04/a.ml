open! Core

let () =
  let passphrases =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_lines file
      |> List.map ~f:(String.split ~on:' '))
  in
  List.count passphrases ~f:(fun phrase ->
    List.find_a_dup phrase
    |> Option.is_none)
  |> printf "%d\n"
;;
