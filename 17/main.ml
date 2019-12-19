open! Core
open! Async
open! Import
open Intcode

let debug = false
let input () = Reader.file_contents "input" >>| Program.of_string

let output program =
  match Program.Async.run program with
  | { input; output; done_ } ->
    let buffer = Buffer.create 0 in
    Pipe.close input;
    let%bind () =
      Pipe.iter_without_pushback output ~f:(fun c ->
        Buffer.add_char buffer (Char.of_int_exn c))
    and () = done_ in
    (* Need the String.strip because there is an empty line at the end of the
       output. *)
    return (Buffer.contents buffer |> String.strip |> String.split_lines |> Array.of_list)
;;

let map =
  Lazy_deferred.create (fun () ->
    let%bind program = input () in
    output program)
;;

let a () =
  let%bind map = Lazy_deferred.force_exn map in
  let sum = ref 0 in
  for i = 1 to Array.length map - 2 do
    for j = 1 to String.length map.(i) - 2 do
      let is_scaffold (x, y) = Char.( = ) map.(i + x).[j + y] '#' in
      if List.for_all [ -1, 0; 1, 0; 0, -1; 0, 1; 0, 0 ] ~f:is_scaffold
      then sum := !sum + (i * j)
    done
  done;
  printf "%d\n" !sum;
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 2660 |}]
;;

let show_map () =
  let%map map = Lazy_deferred.force_exn map in
  Array.iter map ~f:print_endline
;;

let%expect_test "map" =
  let%bind () = show_map () in
  [%expect
    {|
    ..............###########.###########................
    ..............#.........#.#.........#................
    ..............#.......#########.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.......#.#.#...#.....#................
    ..............#.#########.#...#.....#................
    ..............#.#.....#...#...#.....#................
    ..............#.#.....#...#########.#................
    ..............#.#.....#.......#...#.#................
    ..............#########.......#######................
    ................#.................#..................
    ................#...........#########................
    ................#...........#.....#.#................
    ................#########...#.....#.#................
    ........................#...#.....#.#................
    ........................#...#.....#.#................
    ........................#...#.......#................
    ........................#...#.......#######..........
    ........................#...#.............#..........
    ....................#########.............#..........
    ....................#...#.................#..........
    ..................#######.................#..........
    ..................#.#.....................#..........
    ..................#.#.....................#..........
    ..................#.#.....................#..........
    ..................#.#.....................##########^
    ..................#.#................................
    #######...#########.#................................
    #.....#...#.........#................................
    #.....#...#.#########................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#...#.#........................................
    #.....#######........................................
    #.........#..........................................
    ###########.......................................... |}]
;;

let dir_of_char : char -> Robot.Dir.t = function
  | '^' -> N
  | '>' -> E
  | 'v' -> S
  | '<' -> W
  | char -> raise_s [%message "Invalid robot direction char" ~_:(char : char)]
;;

module Route_component = struct
  type t =
    { first_turn : [ `Left | `Right ]
    ; then_go : int
    }
  [@@deriving equal, sexp_of]

  let to_string { first_turn; then_go } =
    sprintf
      "%s,%d"
      (match first_turn with
       | `Left -> "L"
       | `Right -> "R")
      then_go
  ;;
end

(* TODO: Move these into {!Robot.Point}. *)
let ij_of_xy ~height (x, y) = height - 1 - y, x
let xy_of_ij ~height (i, j) = j, height - 1 - i

let find_route map =
  let height = Array.length map in
  let i, j, dir =
    Array.find_mapi_exn map ~f:(fun i line ->
      match String.lfindi line ~f:(fun _ c -> String.mem "^>v<" c) with
      | None -> None
      | Some j -> Some (i, j, dir_of_char line.[j]))
  in
  let route = ref [] in
  let robot =
    Robot.create_with_dir ~initial_loc:(xy_of_ij (i, j) ~height) ~initial_dir:dir
  in
  let try_get ~xy =
    Option.try_with (fun () ->
      let i, j = ij_of_xy xy ~height in
      map.(i).[j])
  in
  let get_rel turn =
    try_get
      ~xy:(Robot.Point.add (Robot.loc robot) (Robot.Dir.turn (Robot.dir robot) turn))
  in
  let rec loop () =
    let first_turn =
      match get_rel `Left, get_rel `Right with
      | Some '#', _ -> Some `Left
      | _, Some '#' -> Some `Right
      | _, _ -> None
    in
    match first_turn with
    | None -> ()
    | Some first_turn ->
      Robot.turn robot first_turn;
      let steps = ref 0 in
      while
        match try_get ~xy:(Robot.loc robot) with
        | None | Some '.' -> false
        | Some _ -> true
      do
        Robot.step_forward robot;
        incr steps
      done;
      Robot.step_backward robot;
      decr steps;
      route := { Route_component.first_turn; then_go = !steps } :: !route;
      loop ()
  in
  loop ();
  List.rev !route
;;

let line_wrap_with_commas strings =
  let fmt = Format.str_formatter in
  Format.pp_set_margin fmt 20;
  List.iter strings ~f:(fun s ->
    Format.pp_print_string fmt s;
    Format.pp_print_char fmt ',';
    Format.pp_print_cut fmt ());
  Format.flush_str_formatter ()
;;

let%expect_test "find_route" =
  let%bind map = Lazy_deferred.force_exn map in
  find_route map
  |> List.map ~f:Route_component.to_string
  |> line_wrap_with_commas
  |> print_endline;
  [%expect
    {|
      L,10,R,8,L,6,R,6,
      L,8,L,8,R,8,L,10,
      R,8,L,6,R,6,R,8,
      L,6,L,10,L,10,L,10,
      R,8,L,6,R,6,L,8,
      L,8,R,8,R,8,L,6,
      L,10,L,10,L,8,L,8,
      R,8,R,8,L,6,L,10,
      L,10,L,8,L,8,R,8, |}]
;;

module Which_program = struct
  module T = struct
    type t =
      | A
      | B
      | C
    [@@deriving compare, enumerate, sexp]
  end

  include T
  include Comparable.Make_plain (T)
  include Sexpable.To_stringable (T)
end

type what_happened =
  | Fail
  | Succeed of Route_component.t list Which_program.Map.t * Which_program.t list

let definition_too_long defn =
  let length =
    List.map defn ~f:Route_component.to_string |> String.concat ~sep:"," |> String.length
  in
  length > 20
;;

let list_chop_prefix list ~prefix ~equal =
  if List.is_prefix list ~prefix ~equal
  then Some (List.drop list (List.length prefix))
  else None
;;

let rec try_to_complete ~route ~programs ~programs_to_define =
  match route with
  | [] -> Succeed (programs, [])
  | _ :: _ as route ->
    (match
       Map.to_sequence programs
       |> Sequence.fold_until
            ~init:()
            ~finish:(fun () -> Fail)
            ~f:(fun () (name, program) ->
              match
                list_chop_prefix route ~prefix:program ~equal:[%equal: Route_component.t]
              with
              | None -> Continue ()
              | Some route ->
                (match try_to_complete ~route ~programs ~programs_to_define with
                 | Fail -> Continue ()
                 | Succeed (programs, programs_called) ->
                   Stop (Succeed (programs, name :: programs_called))))
     with
     | Succeed _ as success -> success
     | Fail ->
       (match programs_to_define with
        | [] -> Fail
        | program_to_define :: programs_to_define ->
          let rec loop route program_defn =
            match route with
            | [] -> Fail
            | next_step :: route ->
              let program_defn = next_step :: program_defn in
              if definition_too_long program_defn
              then Fail
              else (
                match
                  try_to_complete
                    ~route
                    ~programs:
                      (Map.add_exn
                         programs
                         ~key:program_to_define
                         ~data:(List.rev program_defn))
                    ~programs_to_define
                with
                | Fail -> loop route program_defn
                | Succeed (programs, programs_called) ->
                  Succeed (programs, program_to_define :: programs_called))
          in
          loop route []))
;;

let compress ~route =
  match
    try_to_complete
      ~route
      ~programs:Which_program.Map.empty
      ~programs_to_define:Which_program.all
  with
  | Fail -> failwith "couldn't compress"
  | Succeed (programs, programs_called) -> programs, programs_called
;;

let%expect_test "compress" =
  let%bind map = Lazy_deferred.force_exn map in
  let programs, programs_called = compress ~route:(find_route map) in
  print_s
    [%message
      (programs : Route_component.t list Which_program.Map.t)
        (programs_called : Which_program.t list)];
  [%expect
    {|
    ((programs (
       (A (
         ((first_turn Left)  (then_go 10))
         ((first_turn Right) (then_go 8))
         ((first_turn Left)  (then_go 6))
         ((first_turn Right) (then_go 6))))
       (B (
         ((first_turn Left)  (then_go 8))
         ((first_turn Left)  (then_go 8))
         ((first_turn Right) (then_go 8))))
       (C (
         ((first_turn Right) (then_go 8))
         ((first_turn Left)  (then_go 6))
         ((first_turn Left)  (then_go 10))
         ((first_turn Left)  (then_go 10))))))
     (programs_called (A B A C A B C B C B))) |}]
;;

(* Manually computed solution:

   A = L,8,L,8,R,8
   B = L,10,R,8,L,6,R,6
   C = R,8,L,6,L,10,L,10

   L,10,R,8,L,6,R,6,L,8,L,8,R,8,L,10,
   R,8,L,6,R,6,R,8,L,6,L,10,L,10,L,10,
   R,8,L,6,R,6,L,8,L,8,R,8,R,8,L,6,L,10,
   L,10,L,8,L,8,R,8,R,8,L,6,L,10,L,10,L,8,L,8,R,8

   B,A,B,C,B,A,C,A,C,A *)

let b () =
  let%bind map = Lazy_deferred.force_exn map in
  let programs, programs_called = compress ~route:(find_route map) in
  let%bind program = input () in
  program.memory.(0) <- 2;
  match Program.Async.run program with
  | { input; output; done_ = _ } ->
    let write_strings strings =
      String.iter
        (String.concat strings ~sep:"," ^ "\n")
        ~f:(fun c -> Pipe.write_without_pushback input (Char.to_int c))
    in
    write_strings (programs_called |> List.map ~f:Which_program.to_string);
    Map.iter programs ~f:(fun program_defn ->
      write_strings (program_defn |> List.map ~f:Route_component.to_string));
    if debug
    then (
      write_strings [ "y" ];
      Pipe.iter_without_pushback output ~f:(fun c -> print_char (Char.of_int_exn c)))
    else (
      write_strings [ "n" ];
      Pipe.iter_without_pushback output ~f:(fun c ->
        if c > Char.to_int Char.max_value then printf "%d\n" c))
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 790595 |}]
;;
