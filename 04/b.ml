open! Core

let () =
  let passphrases =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_lines file
      |> List.map ~f:(String.split ~on:' '))
  in
  List.count passphrases ~f:(fun phrase ->
    phrase
    |> List.map ~f:(fun string ->
      String.to_list string
      |> List.sort ~cmp:Char.compare
      |> String.of_char_list)
    |> List.find_a_dup ~compare:String.compare
    |> Option.is_none)
  |> printf "%d\n"
;;
