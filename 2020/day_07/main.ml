open! Core
open! Async
open! Import

module Input = struct
  type t = (string, (int * string) list) Hashtbl.t [@@deriving sexp_of]

  let rule_re, contained_re =
    let open Re in
    let contained = {|(\d+) ([\w ]+) bags?|} in
    ( compile
        (Perl.re
           [%string
             {|([\w ]+) bags contain (no other bags|(%{contained}(, %{contained})*))[.]|}])
    , compile (Perl.re contained) )
  ;;

  let parse input : t =
    input
    |> String.split_lines
    |> List.map ~f:(fun line ->
      let g = Re.exec rule_re line in
      let from_color = Re.Group.get g 1 in
      let contained = Re.Group.get g 2 in
      let to_colors =
        match contained with
        | "no other bags" -> []
        | _ ->
          Re.split Re.(compile (str ", ")) contained
          |> List.map ~f:(fun contained ->
            let g = Re.exec contained_re contained in
            let count = Re.Group.get g 1 |> Int.of_string in
            let color = Re.Group.get g 2 in
            count, color)
      in
      from_color, to_colors)
    |> Hashtbl.of_alist_exn (module String)
  ;;

  let t : t Lazy_deferred.t =
    Lazy_deferred.create (fun () -> Reader.file_contents "input.txt" >>| parse)
  ;;
end

let rec find_bags_containing rules ~bag =
  let next =
    Hashtbl.to_alist rules
    |> List.filter_map ~f:(fun (from, to_) ->
      if List.exists to_ ~f:(fun (_, bag') -> String.equal bag bag')
      then Some from
      else None)
  in
  next @ List.concat_map next ~f:(fun bag -> find_bags_containing rules ~bag)
;;

let a () =
  let%bind rules = Lazy_deferred.force_exn Input.t in
  let bags =
    find_bags_containing rules ~bag:"shiny gold" |> Set.of_list (module String)
  in
  let bags = Set.remove bags "shiny gold" in
  print_s [%sexp (Set.length bags : int)];
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 144 |}];
  return ()
;;

let rec count_bags rules ~bag =
  1
  +
  match Hashtbl.find rules bag with
  | None -> 0
  | Some contained ->
    List.sum
      (module Int)
      contained
      ~f:(fun (count, bag') -> count * count_bags rules ~bag:bag')
;;

let b () =
  let%bind rules = Lazy_deferred.force_exn Input.t in
  count_bags rules ~bag:"shiny gold" - 1 |> [%sexp_of: int] |> print_s;
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 5956 |}];
  return ()
;;
