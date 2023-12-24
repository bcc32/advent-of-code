open! Core
open! Async
open! Import

let input =
  Lazy_deferred.create (fun () ->
    Reader.file_contents "aoc.in"
    >>| String.split_lines
    >>| List.group ~break:(fun _ ->
        function
        | "" -> true
        | _ -> false)
    >>| List.map ~f:(List.filter ~f:(Fn.non String.is_empty))
    >>| List.map ~f:(List.concat_map ~f:(String.split ~on:' ')))
;;

let required_fields =
  let open Re in
  [ ( "byr"
    , group (repn digit 4 (Some 4))
    , fun x -> x.(1) |> Int.of_string |> Int.between ~low:1920 ~high:2002 )
  ; ( "iyr"
    , group (repn digit 4 (Some 4))
    , fun x -> x.(1) |> Int.of_string |> Int.between ~low:2010 ~high:2020 )
  ; ( "eyr"
    , group (repn digit 4 (Some 4))
    , fun x -> x.(1) |> Int.of_string |> Int.between ~low:2020 ~high:2030 )
  ; ( "hgt"
    , seq [ group (rep digit); group (alt [ str "cm"; str "in" ]) ]
    , fun x ->
        let y = Int.of_string x.(1) in
        match x.(2) with
        | "cm" -> Int.between y ~low:150 ~high:193
        | "in" -> Int.between y ~low:59 ~high:76
        | _ -> assert false )
  ; ( "hcl"
    , seq [ char '#'; repn (alt [ rg '0' '9'; rg 'a' 'f' ]) 6 (Some 6) ]
    , fun _ -> true )
  ; ( "ecl"
    , alt [ str "amb"; str "blu"; str "brn"; str "gry"; str "grn"; str "hzl"; str "oth" ]
    , fun _ -> true )
  ; ("pid", repn digit 9 (Some 9), fun _ -> true)
  ]
  |> List.map ~f:(fun (f, r, p) -> f, Re.compile (seq [ bos; r; eos ]), p)
;;

let is_valid lines =
  let fields =
    lines
    |> List.map ~f:(fun field ->
      let k, _ = String.lsplit2_exn field ~on:':' in
      k)
  in
  List.for_all required_fields ~f:(fun (f, _, _) -> List.mem fields f ~equal:String.equal)
;;

let a () =
  let%bind input = Lazy_deferred.force_exn input in
  List.count input ~f:is_valid |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 200 |}];
  return ()
;;

let is_valid lines =
  let fields = lines |> List.map ~f:(fun field -> String.lsplit2_exn field ~on:':') in
  List.for_all required_fields ~f:(fun (f, r, p) ->
    List.exists fields ~f:(fun (k, v) ->
      String.equal k f
      &&
      match Re.exec_opt r v with
      | None -> false
      | Some g -> p (Re.Group.all g)))
;;

let b () =
  let%bind input = Lazy_deferred.force_exn input in
  List.count input ~f:is_valid |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 116 |}];
  return ()
;;
