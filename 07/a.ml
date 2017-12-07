open! Core

let () =
  let lines =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_lines file
      |> List.map ~f:(fun line ->
        let word = String.split line ~on:' ' |> List.hd_exn in
        ( word
        , match String.split line ~on:'>' with
        | [ _; rhs ] ->
          rhs
          |> String.split ~on:','
          |> List.map ~f:String.strip
        | _ -> []
        )))
  in
  let found = String.Hash_set.create () in
  List.iter lines ~f:(fun (w, _) -> Hash_set.add found w);
  List.iter lines ~f:(fun (_, words) ->
    List.iter words ~f:(fun w ->
    Hash_set.remove found w));
  match Hash_set.to_list found with
  | [ word ] -> printf "%s\n" word
  | _ -> failwith "expected 1 word"
;;
