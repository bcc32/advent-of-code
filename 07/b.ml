open! Core

let rec weight =
  let cache = String.Table.create () in
  fun name lines ->
    Hashtbl.find_or_add cache name ~default:(fun () ->
      let (_, w, words) = List.find_exn lines ~f:(fun (w, _, _) -> w = name) in
      w + List.sum (module Int) words ~f:(fun w -> weight w lines))
;;

let self_weight : string -> (string * int * string list) list -> int =
  let cache = String.Table.create () in
  fun name lines ->
    Hashtbl.find_or_add cache name ~default:(fun () ->
      let (_, w, _) = List.find_exn lines ~f:(fun (w, _, _) -> w = name) in
      w)
;;

let () =
  let lines =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_lines file
      |> List.map ~f:(fun line ->
        let word = String.split line ~on:' ' |> List.hd_exn in
        let weight =
          String.split_on_chars line ~on:['('; ')']
          |> Fn.flip List.nth_exn 1
          |> Int.of_string
        in
        ( word
        , weight
        , match String.split line ~on:'>' with
        | [ _; rhs ] ->
          rhs
          |> String.split ~on:','
          |> List.map ~f:String.strip
        | _ -> []
        )))
  in
  List.iter lines ~f:(fun (_, _, words) ->
    let weights = List.map words ~f:(fun w -> weight w lines) in
    match List.dedup weights with
    | [] -> ()
    | [ _ ] -> ()
    | [ a; b ] as ddw ->
      List.iter ddw ~f:(fun w ->
        if List.count weights ~f:((=) w) = 1
        then (
          let other_weight = w lxor a lxor b in
          let current_weight =
            List.find_map_exn words ~f:(fun c ->
              if weight c lines = w
              then (Some (self_weight c lines))
              else None)
          in
          Debug.eprintf "%d" (current_weight + (other_weight - w))))
    | _ -> assert false);
;;
