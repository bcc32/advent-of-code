open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let a () =
  let%bind program = input () in
  match Program.Async.run program with
  | { input; output; done_ } ->
    let print_line line =
      String.iter (line ^ "\n") ~f:(fun c ->
        Pipe.write_without_pushback input (Char.to_int c))
    in
    print_line "NOT A J";
    print_line "NOT B T";
    print_line "OR T J";
    print_line "NOT C T";
    print_line "OR T J";
    print_line "AND D J";
    print_line "WALK";
    let%bind () =
      Pipe.iter_without_pushback output ~f:(fun c ->
        if c > 255 then printf "%d\n" c else print_char (Char.of_int_exn c))
    in
    done_
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {|
    Input instructions:

    Walking...

    19348840 |}]
;;

(* Can reach and make progress from:

   9: I
   8: H and I
   7: G and (H and I)
   6: F and (G and (H and I))
   5: E and (I or (F and (G and (H and I))))
   = E and I
   4: D and (H or (E and I))
   3: C and (G or (D and (H or (E and I))))
   2: B and (F or (C and (G or (D and (H or (E and I))))))
   1: A and (E or (B and (F or (C and (G or (D and (H or (E and I))))))))

   Assuming 6,7,8,9 have somewhere to jump to:

   9: I
   8: H
   7: G
   6: F
   5: E and (I or F)
   4: D and (H or (E and (I or F)))
   3: C and (G or (D and (H or (E and (I or F)))))
   2: B and (F or (C and (G or (D and (H or (E and (I or F)))))))
   1: A and (E or (B and (F or (C and (G or (D and (H or (E and (I or F)))))))))

   Trying to do it by hand doesn't seem to work. *)

(* Jump rules:

   Jump only if:

   1. D                         -- jump destination has ground
   2. E or H                    -- can proceed from destination
   3. not (A and B and C and D) -- no need to jump *)

let b () =
  let%bind program = input () in
  match Program.Async.run program with
  | { input; output; done_ } ->
    let print_line line =
      String.iter (line ^ "\n") ~f:(fun c ->
        Pipe.write_without_pushback input (Char.to_int c))
    in
    print_line "NOT J J";
    print_line "AND D J";
    print_line "OR A T";
    print_line "AND B T";
    print_line "AND C T";
    print_line "AND D T";
    print_line "NOT T T";
    print_line "AND T J";
    print_line "AND E T";
    print_line "OR E T";
    print_line "OR H T";
    print_line "AND T J";
    print_line "RUN";
    let%bind () =
      Pipe.iter_without_pushback output ~f:(fun c ->
        if c > 255 then printf "%d\n" c else print_char (Char.of_int_exn c))
    in
    done_
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {|
    Input instructions:

    Running...

    1141857182 |}]
;;
