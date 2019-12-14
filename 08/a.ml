open! Core

let relational_of_string = function
  | "<" -> Int.( < )
  | ">" -> Int.( > )
  | ">=" -> Int.( >= )
  | "<=" -> Int.( <= )
  | "==" -> Int.( = )
  | "!=" -> Int.( <> )
  | s -> invalid_arg s
;;

let () =
  let input =
    In_channel.with_file
      (Sys.get_argv ()).(1)
      ~f:(fun file ->
        In_channel.input_lines file
        |> List.map ~f:(fun line ->
          match String.split line ~on:' ' with
          | [ reg; ins; amt; _; other; rel; lit ] ->
            ( reg
            , (if String.( = ) ins "inc" then 1 else -1)
            , Int.of_string amt
            , other
            , relational_of_string rel
            , Int.of_string lit )
          | _ -> invalid_arg line))
  in
  let registers = String.Table.create () in
  List.iter input ~f:(fun (reg, factor, amt, other, f, lit) ->
    let other = Hashtbl.find_or_add registers other ~default:(fun () -> 0) in
    let reg_cur = Hashtbl.find_or_add registers reg ~default:(fun () -> 0) in
    if f other lit then Hashtbl.set registers ~key:reg ~data:((factor * amt) + reg_cur));
  Hashtbl.data registers |> List.max_elt ~compare:Int.compare |> uw |> printf "%d\n"
;;
