open! Core
open! Async
open! Import

module Rule = struct
  type t =
    | Literal of string
    | Alt of int list list
  [@@deriving sexp_of]

  let of_string line =
    let [ index; rule ] = line |> Advent_of_code_input_helpers.words ~sep:": " in
    ( index |> Int.of_string
    , if String.is_prefix rule ~prefix:{|"|}
      then Literal (String.sub rule ~pos:1 ~len:(String.length rule - 2))
      else
        Alt
          (rule
           |> Advent_of_code_input_helpers.words ~sep:" | "
           |> List.map
                ~f:
                  (Advent_of_code_input_helpers.words ~sep:" " >> List.map ~f:Int.of_string)
          ) )
  ;;

  exception Failed

  let rec try_consume t ~rules ~pos message ~new_rules ~rule_index ~f =
    match rule_index with
    | 8 when new_rules ->
      let rec loop_try_rule_42 pos =
        try_consume
          (Map.find_exn rules 42)
          ~pos
          ~rules
          message
          ~new_rules
          ~rule_index:42
          ~f:(fun pos' ->
            if pos' > pos
            then (
              f pos';
              loop_try_rule_42 pos'))
      in
      loop_try_rule_42 pos
    | 11 when new_rules ->
      let rec loop_try_count count =
        match
          try_consume
            (Map.find_exn rules 42)
            ~pos
            ~rules
            message
            ~new_rules
            ~rule_index:42
            ~f:(fun pos' ->
              if pos' <= pos
              then raise Failed
              else
                try_consume
                  (Map.find_exn rules 31)
                  ~pos
                  ~rules
                  message
                  ~new_rules
                  ~rule_index:31
                  ~f:(fun pos'' -> if pos'' <= pos then raise Failed else f pos''))
        with
        | () -> loop_try_count (count + 1)
        | exception Failed -> ()
      in
      loop_try_count 1
    | rule_index ->
      (match t with
       | Literal prefix ->
         if String.is_substring_at message ~pos ~substring:prefix
         then f (pos + String.length prefix)
       | Alt alternatives ->
         let original_pos = pos in
         List.iter alternatives ~f:(fun rules_indices ->
           let rec loop rules_indices pos =
             match rules_indices with
             | [] -> if pos > original_pos then f pos
             | rule_index :: tl ->
               try_consume
                 (Map.find_exn rules rule_index)
                 ~rules
                 ~pos
                 ~new_rules
                 ~rule_index
                 message
                 ~f:(fun pos' -> loop tl pos')
           in
           loop rules_indices pos))
  ;;

  let does_match ~rules message ~new_rules =
    with_return (fun { return } ->
      try_consume
        (Map.find_exn rules 0)
        ~rules
        ~pos:0
        message
        ~new_rules
        ~rule_index:0
        ~f:(fun pos -> if pos = String.length message then return true);
      false)
  ;;

  let rec all_matching_strings ~rules =
    let cache = Hashtbl.create (module Int) in
    fun ~rule_index : String.Set.t Or_error.t ->
      Hashtbl.findi_or_add cache rule_index ~default:(fun rule_index ->
        match Map.find_exn rules rule_index with
        | Literal s -> Ok (String.Set.singleton s)
        | Alt alternatives ->
          if List.exists alternatives ~f:(fun a ->
            List.mem a rule_index ~equal:Int.equal)
          then error_s [%message "recursive rule"]
          else
            List.map alternatives ~f:(fun rule_indices ->
              List.map rule_indices ~f:(fun rule_index ->
                all_matching_strings ~rules ~rule_index)
              |> List.fold ~init:(String.Set.singleton "") ~f:(fun accum next ->
                Set.to_list accum
                |> List.concat_map ~f:(fun so_far ->
                  match next with
                  | Error _ -> []
                  | Ok next ->
                    Set.to_list next
                    |> List.map ~f:(fun next -> so_far ^ next))
                |> String.Set.of_list))
            |> String.Set.union_list
            |> Ok)
  ;;

  let cache = Hashtbl.create (module Int)

  let rec get_parser_for_rule ~rules ~rule_index : unit Angstrom.t =
    Hashtbl.findi_or_add cache rule_index ~default:(fun rule_index ->
      let open Angstrom in
      match rule_index with
      | 0 ->
        let forty_two = get_parser_for_rule ~rules ~rule_index:42 in
        let thirty_one = get_parser_for_rule ~rules ~rule_index:31 in
        fix (fun forty_two_then_thirty_one ->
          forty_two *> forty_two_then_thirty_one
          <|> forty_two
              *> fix (fun equal_count ->
                forty_two *> equal_count *> thirty_one
                <|> forty_two *> thirty_one))
      | 8 ->
        let sub = get_parser_for_rule ~rules ~rule_index:42 in
        skip_many1 sub <?> "8"
      | 11 ->
        let sub1 = get_parser_for_rule ~rules ~rule_index:42 in
        let sub2 = get_parser_for_rule ~rules ~rule_index:31 in
        fix (fun eleven ->
          sub1 *> eleven *> sub2 <?> "11 recur" <|> (sub1 *> sub2 <?> "11 base"))
        <?> "11"
      | 42 | 31 ->
        let strings = all_matching_strings ~rules ~rule_index |> ok_exn in
        choice
          (strings
           |> Set.to_list
           |> List.map ~f:string
           |> List.map ~f:(fun a -> a >>| ignore))
      | rule_index ->
        (match Map.find_exn rules rule_index with
         | Literal s -> string s >>| ignore
         | Alt alternatives ->
           List.map alternatives ~f:(fun rule_indices ->
             List.map rule_indices ~f:(fun rule_index ->
               get_parser_for_rule ~rules ~rule_index)
             |> list)
           |> choice ~failure_msg:(sprintf "failed alternative rule %d" rule_index)
           >>| ignore)
        <?> Int.to_string rule_index)
  ;;
end

module Input = struct
  open! Advent_of_code_input_helpers

  type t = Rule.t Int.Map.t * string list [@@deriving sexp_of]

  let parse input : t =
    let [ rules; messages ] = input |> lines |> paragraphs in
    let rules = rules |> List.map ~f:Rule.of_string |> Int.Map.of_alist_exn in
    rules, messages
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let%expect_test "all_matching_strings_for_42" =
  let%bind rules, messages = Lazy_deferred.force_exn Input.t in
  Rule.all_matching_strings ~rules ~rule_index:42
  |> [%sexp_of: String.Set.t Or_error.t]
  |> print_s;
  [%expect
    {|
    (Ok (
      aaaaaaaa
      aaaaaabb
      aaaaabaa
      aaaaabba
      aaaaabbb
      aaaabaaa
      aaaabaab
      aaaababb
      aaaabbaa
      aaaabbab
      aaaabbbb
      aaabaaaa
      aaabaaab
      aaababaa
      aaababab
      aaababbb
      aaabbaab
      aaabbbaa
      aaabbbba
      aabaaaba
      aabaaabb
      aabaabaa
      aabababb
      aababbab
      aababbba
      aabbaaab
      aabbaabb
      aabbabaa
      aabbabbb
      aabbbaba
      aabbbabb
      aabbbbba
      aabbbbbb
      abaaaaaa
      abaabaaa
      abaabaab
      abaababb
      abaabbaa
      abaabbab
      abaabbba
      ababaaaa
      ababaaba
      ababaabb
      abababaa
      abababba
      ababbabb
      ababbbaa
      ababbbbb
      abbaabba
      abbabbaa
      abbabbba
      abbabbbb
      abbbaaab
      abbbaaba
      abbbaabb
      abbbabba
      abbbbaaa
      abbbbaba
      abbbbabb
      abbbbbaa
      abbbbbba
      abbbbbbb
      baaaaaaa
      baaaaaab
      baaaaabb
      baaaabaa
      baaaabab
      baaaabbb
      baaabaaa
      baaabaab
      baaabbab
      baaabbba
      baaabbbb
      baababaa
      baababba
      baabbaab
      babaabaa
      babaabab
      babaabba
      babaabbb
      bababaaa
      bababaab
      babababa
      bababbaa
      bababbbb
      babbaaba
      babbaabb
      babbabab
      babbbaaa
      babbbaab
      babbbaba
      babbbbaa
      babbbbab
      babbbbba
      babbbbbb
      bbaaaaaa
      bbaaabaa
      bbaaabab
      bbaaabbb
      bbaabaab
      bbaababa
      bbaabbaa
      bbaabbab
      bbaabbbb
      bbabaaaa
      bbabaaab
      bbabaaba
      bbababab
      bbababba
      bbabbaba
      bbabbabb
      bbabbbaa
      bbbaaaba
      bbbaaabb
      bbbaabab
      bbbaabba
      bbbababb
      bbbabbaa
      bbbabbab
      bbbabbbb
      bbbbaaaa
      bbbbaaba
      bbbbabaa
      bbbbabba
      bbbbabbb
      bbbbbabb
      bbbbbbaa
      bbbbbbab)) |}]
;;

let a () =
  let%bind rules, messages = Lazy_deferred.force_exn Input.t in
  List.count messages ~f:(Rule.does_match ~rules ~new_rules:false)
  |> [%sexp_of: int]
  |> print_s;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  let%bind () = [%expect {| 272 |}] in
  return ()
;;

let b () =
  let%bind rules, messages = Lazy_deferred.force_exn Input.t in
  (* let parser = Rule.get_parser_for_rule ~rules ~rule_index:0 in *)
  assert (
    Set.are_disjoint
      (Rule.all_matching_strings ~rules ~rule_index:31 |> ok_exn)
      (Rule.all_matching_strings ~rules ~rule_index:42 |> ok_exn));
  let len =
    Set.choose_exn (Rule.all_matching_strings ~rules ~rule_index:31 |> ok_exn)
    |> String.length
  in
  assert (
    Set.for_all
      (Rule.all_matching_strings ~rules ~rule_index:31 |> ok_exn)
      ~f:(String.length >> ( = ) len));
  assert (
    Set.for_all
      (Rule.all_matching_strings ~rules ~rule_index:42 |> ok_exn)
      ~f:(String.length >> ( = ) len));
  let does_match message =
    let rec try_forty_two forty_two_count ~pos =
      if pos = String.length message
      then false
      else if Set.exists
                (Rule.all_matching_strings ~rules ~rule_index:42 |> ok_exn)
                ~f:(fun s -> String.is_substring_at message ~substring:s ~pos)
      then try_forty_two (forty_two_count + 1) ~pos:(pos + len)
      else try_thirty_one forty_two_count ~pos
    and try_thirty_one count_to_go ~pos =
      if count_to_go <= 0
      then false
      else if pos = String.length message
      then true
      else if Set.exists
                (Rule.all_matching_strings ~rules ~rule_index:31 |> ok_exn)
                ~f:(fun s -> String.is_substring_at message ~substring:s ~pos)
      then try_thirty_one (count_to_go - 1) ~pos:(pos + len)
      else false
    in
    try_forty_two 0 ~pos:0
  in
  List.count messages ~f:does_match |> [%sexp_of: int] |> print_s;
  (* List.count messages ~f:(fun m ->
   *   match
   *     Angstrom.parse_string
   *       ~consume:All
   *       (Angstrom.consumed parser |> Angstrom.map ~f:String.length)
   *       m
   *   with
   *   | Ok n ->
   *     print_s [%sexp (n : int), (String.length m : int), (m : string)];
   *     true
   *   | Error e ->
   *     print_s [%message "error" (e : string) (m : string)];
   *     false)
   * |> [%sexp_of: int]
   * |> print_s; *)
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  let%bind () = [%expect {| 374 |}] in
  return ()
;;

let%expect_test "testing my understanding" =
  let parser =
    let open Angstrom in
    skip_many1 (string "AA") *> string "AAC"
  in
  Angstrom.parse_string ~consume:All parser "AAAAC"
  |> [%sexp_of: (string, string) Result.t]
  |> print_s;
  [%expect {| (Error ": not enough input") |}]
;;
